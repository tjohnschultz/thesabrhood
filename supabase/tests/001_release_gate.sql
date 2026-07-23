begin;

do $$
declare
  run_id uuid;
  release_id uuid;
  allowed_release_id uuid;
  batch_id uuid;
  game_id uuid;
  event_id uuid;
  blocked boolean := false;
begin
  insert into observability.pipeline_runs (
    pipeline_key, trigger_kind, status, finished_at
  ) values (
    'migration-smoke-test', 'local', 'succeeded', now()
  ) returning id into run_id;

  insert into publishing.releases (release_key, pipeline_run_id)
  values ('migration-smoke-test', run_id)
  returning id into release_id;

  insert into publishing.release_sources (
    release_id, provider_key, source_through, row_count, checksum_sha256
  ) values (
    release_id,
    'fangraphs',
    current_date,
    0,
    repeat('0', 64)
  );

  insert into observability.release_checks (
    release_id, check_key, status
  ) values (
    release_id, 'minimum_release_contract', 'pass'
  );

  begin
    perform publishing.publish_release(release_id);
  exception
    when others then
      if position('not approved for commercial serving' in sqlerrm) = 0 then
        raise;
      end if;
      blocked := true;
  end;

  if not blocked then
    raise exception 'Permission gate failed: restricted FanGraphs source was published';
  end if;

  insert into ingest.batches (
    provider_key,
    pipeline_run_id,
    source_uri,
    source_version,
    source_sha256,
    effective_from,
    effective_through,
    row_count,
    status
  ) values (
    'retrosheet',
    run_id,
    'fixture://release-gate',
    'test',
    repeat('1', 64),
    current_date,
    current_date,
    1,
    'accepted'
  ) returning id into batch_id;

  insert into canonical.games (
    source_provider_key,
    source_game_id,
    played_on,
    home_team_code,
    away_team_code,
    source_batch_id
  ) values (
    'retrosheet',
    'RELEASE_GATE_TEST',
    current_date,
    'HOM',
    'AWY',
    batch_id
  ) returning id into game_id;

  insert into canonical.game_events (
    game_id,
    source_provider_key,
    source_event_id,
    event_sequence,
    inning,
    batting_side,
    event_text,
    source_batch_id
  ) values (
    game_id,
    'retrosheet',
    'RELEASE_GATE_TEST:1',
    1,
    1,
    0,
    'S7/G',
    batch_id
  ) returning id into event_id;

  insert into publishing.releases (release_key, pipeline_run_id)
  values ('migration-smoke-test-allowed', run_id)
  returning id into allowed_release_id;

  insert into publishing.release_sources (
    release_id, provider_key, source_through, row_count, checksum_sha256
  ) values (
    allowed_release_id,
    'retrosheet',
    current_date,
    1,
    repeat('1', 64)
  );

  insert into publishing.release_games (release_id, game_id, audience)
  values (allowed_release_id, game_id, 'public');

  insert into publishing.release_game_events (
    release_id, game_event_id, audience
  ) values (
    allowed_release_id, event_id, 'public'
  );

  insert into observability.release_checks (
    release_id, check_key, status
  ) values (
    allowed_release_id, 'minimum_release_contract', 'pass'
  );

  perform publishing.publish_release(allowed_release_id);

  if not exists (
    select 1
    from publishing.current_release
    where singleton = true and release_id = allowed_release_id
  ) then
    raise exception 'Atomic publication pointer did not move to the approved release';
  end if;
end;
$$;

rollback;
