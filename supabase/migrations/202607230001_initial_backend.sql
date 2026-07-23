begin;

create schema if not exists extensions;
create extension if not exists pgcrypto with schema extensions;

create schema if not exists ingest;
create schema if not exists canonical;
create schema if not exists publishing;
create schema if not exists observability;
create schema if not exists billing;
create schema if not exists api;

revoke all on schema ingest, canonical, publishing, observability, billing from public, anon, authenticated;
grant usage on schema api to anon, authenticated, service_role;
grant usage on schema ingest, canonical, publishing, observability, billing to anon, authenticated;
grant usage on schema ingest, canonical, publishing, observability, billing to service_role;

create table ingest.providers (
  provider_key text primary key,
  display_name text not null,
  homepage_url text not null,
  terms_url text,
  permission_status text not null
    check (permission_status in ('approved', 'restricted', 'pending_review')),
  commercial_serving_allowed boolean not null default false,
  required_attribution text,
  permission_notes text not null,
  permission_documented_at timestamptz,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  check (
    commercial_serving_allowed = false
    or (permission_status = 'approved' and permission_documented_at is not null)
  )
);

comment on table ingest.providers is
  'The enforceable source-permission registry. A provider cannot enter a public/member release unless commercial_serving_allowed is true.';

insert into ingest.providers (
  provider_key,
  display_name,
  homepage_url,
  terms_url,
  permission_status,
  commercial_serving_allowed,
  required_attribution,
  permission_notes,
  permission_documented_at
) values
  (
    'retrosheet',
    'Retrosheet',
    'https://www.retrosheet.org/',
    'https://www.retrosheet.org/game.htm',
    'approved',
    true,
    'The information used here was obtained free of charge from and is copyrighted by Retrosheet. Interested parties may contact Retrosheet at 20 Sunset Rd., Newark, DE 19711.',
    'Retrosheet permits commercial use and requires its published attribution statement.',
    '2026-07-23T00:00:00Z'
  ),
  (
    'mlb',
    'MLB / Statcast',
    'https://www.mlb.com/',
    null,
    'pending_review',
    false,
    null,
    'Existing research products may remain on the static site. Database serving is blocked until written permission and applicable terms are documented.',
    null
  ),
  (
    'fangraphs',
    'FanGraphs',
    'https://www.fangraphs.com/',
    null,
    'pending_review',
    false,
    null,
    'Existing research products may remain on the static site. Database serving is blocked until written permission and applicable terms are documented.',
    null
  ),
  (
    'sabrhood',
    'The SABRhood',
    'https://thesabrhood.com/',
    null,
    'approved',
    true,
    null,
    'First-party editorial and derived data whose upstream sources are separately approved.',
    '2026-07-23T00:00:00Z'
  );

create table observability.pipeline_runs (
  id uuid primary key default gen_random_uuid(),
  pipeline_key text not null,
  trigger_kind text not null
    check (trigger_kind in ('local', 'manual', 'scheduled', 'backfill')),
  status text not null default 'running'
    check (status in ('running', 'succeeded', 'failed', 'cancelled')),
  git_sha text,
  execution_ref text,
  started_at timestamptz not null default now(),
  finished_at timestamptz,
  source_through date,
  rows_read bigint not null default 0 check (rows_read >= 0),
  rows_written bigint not null default 0 check (rows_written >= 0),
  error_class text,
  error_message text,
  metadata jsonb not null default '{}'::jsonb,
  check (
    (status = 'running' and finished_at is null)
    or (status <> 'running' and finished_at is not null)
  )
);

create index pipeline_runs_pipeline_started_idx
  on observability.pipeline_runs (pipeline_key, started_at desc);

create table ingest.batches (
  id uuid primary key default gen_random_uuid(),
  provider_key text not null references ingest.providers(provider_key),
  pipeline_run_id uuid not null references observability.pipeline_runs(id),
  source_uri text not null,
  source_version text,
  source_sha256 text not null check (source_sha256 ~ '^[0-9a-f]{64}$'),
  acquired_at timestamptz not null default now(),
  effective_from date,
  effective_through date,
  row_count bigint not null default 0 check (row_count >= 0),
  status text not null default 'staged'
    check (status in ('staged', 'accepted', 'rejected')),
  unique (provider_key, source_sha256)
);

create table ingest.raw_records (
  batch_id uuid not null references ingest.batches(id) on delete restrict,
  record_type text not null,
  provider_record_id text not null,
  payload jsonb not null,
  payload_sha256 text not null check (payload_sha256 ~ '^[0-9a-f]{64}$'),
  ingested_at timestamptz not null default now(),
  primary key (batch_id, record_type, provider_record_id)
);

comment on table ingest.raw_records is
  'Immutable provider records. Corrected source material arrives in a new batch rather than overwriting prior evidence.';

create table canonical.teams (
  id uuid primary key default gen_random_uuid(),
  team_code text not null,
  name text not null,
  league text,
  valid_from date not null,
  valid_through date,
  created_at timestamptz not null default now(),
  unique (team_code, valid_from),
  check (valid_through is null or valid_through >= valid_from)
);

create table canonical.people (
  id uuid primary key default gen_random_uuid(),
  display_name text,
  birth_date date,
  created_at timestamptz not null default now()
);

create table canonical.external_ids (
  provider_key text not null references ingest.providers(provider_key),
  entity_kind text not null check (entity_kind in ('team', 'person', 'game')),
  provider_id text not null,
  canonical_id uuid not null,
  first_batch_id uuid not null references ingest.batches(id),
  created_at timestamptz not null default now(),
  primary key (provider_key, entity_kind, provider_id),
  unique (provider_key, entity_kind, canonical_id)
);

create table canonical.games (
  id uuid primary key default gen_random_uuid(),
  source_provider_key text not null references ingest.providers(provider_key),
  source_game_id text not null,
  played_on date not null,
  game_number smallint not null default 0 check (game_number between 0 and 9),
  home_team_id uuid references canonical.teams(id),
  away_team_id uuid references canonical.teams(id),
  home_team_code text not null,
  away_team_code text not null,
  site_code text,
  day_night text,
  completion_status text not null default 'complete'
    check (completion_status in ('scheduled', 'in_progress', 'complete', 'suspended', 'cancelled')),
  source_batch_id uuid not null references ingest.batches(id),
  metadata jsonb not null default '{}'::jsonb,
  created_at timestamptz not null default now(),
  unique (source_provider_key, source_game_id),
  check (home_team_code <> away_team_code)
);

create index games_played_on_idx on canonical.games (played_on desc);

create table canonical.game_events (
  id uuid primary key default gen_random_uuid(),
  game_id uuid not null references canonical.games(id) on delete restrict,
  source_provider_key text not null references ingest.providers(provider_key),
  source_event_id text not null,
  event_sequence integer not null check (event_sequence >= 0),
  inning smallint check (inning > 0),
  batting_side smallint check (batting_side in (0, 1)),
  batter_source_id text,
  count_state text,
  pitch_sequence text,
  event_text text not null,
  source_batch_id uuid not null references ingest.batches(id),
  metadata jsonb not null default '{}'::jsonb,
  created_at timestamptz not null default now(),
  unique (source_provider_key, source_event_id),
  unique (game_id, event_sequence)
);

create index game_events_game_sequence_idx
  on canonical.game_events (game_id, event_sequence);

create table publishing.releases (
  id uuid primary key default gen_random_uuid(),
  release_key text not null unique,
  pipeline_run_id uuid not null references observability.pipeline_runs(id),
  status text not null default 'staged'
    check (status in ('staged', 'published', 'superseded', 'rejected')),
  notes text,
  staged_at timestamptz not null default now(),
  published_at timestamptz,
  rejected_at timestamptz,
  check (
    (status = 'published' and published_at is not null)
    or status <> 'published'
  )
);

create table publishing.release_sources (
  release_id uuid not null references publishing.releases(id) on delete restrict,
  provider_key text not null references ingest.providers(provider_key),
  source_through date not null,
  row_count bigint not null check (row_count >= 0),
  checksum_sha256 text not null check (checksum_sha256 ~ '^[0-9a-f]{64}$'),
  primary key (release_id, provider_key)
);

create table publishing.release_games (
  release_id uuid not null references publishing.releases(id) on delete restrict,
  game_id uuid not null references canonical.games(id) on delete restrict,
  audience text not null check (audience in ('public', 'member')),
  primary key (release_id, game_id, audience)
);

create table publishing.release_game_events (
  release_id uuid not null references publishing.releases(id) on delete restrict,
  game_event_id uuid not null references canonical.game_events(id) on delete restrict,
  audience text not null check (audience in ('public', 'member')),
  primary key (release_id, game_event_id, audience)
);

create table publishing.current_release (
  singleton boolean primary key default true check (singleton),
  release_id uuid references publishing.releases(id),
  switched_at timestamptz not null default now()
);

insert into publishing.current_release (singleton, release_id)
values (true, null);

create table observability.release_checks (
  id uuid primary key default gen_random_uuid(),
  release_id uuid not null references publishing.releases(id) on delete cascade,
  check_key text not null,
  status text not null check (status in ('pass', 'warn', 'fail')),
  observed_value text,
  expected_value text,
  details jsonb not null default '{}'::jsonb,
  checked_at timestamptz not null default now(),
  unique (release_id, check_key)
);

create table billing.profiles (
  user_id uuid primary key references auth.users(id) on delete cascade,
  display_name text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

create table billing.customer_accounts (
  id uuid primary key default gen_random_uuid(),
  user_id uuid not null references auth.users(id) on delete cascade,
  payment_provider text not null,
  external_customer_id text not null,
  created_at timestamptz not null default now(),
  unique (payment_provider, external_customer_id),
  unique (user_id, payment_provider)
);

create table billing.subscriptions (
  id uuid primary key default gen_random_uuid(),
  customer_account_id uuid not null references billing.customer_accounts(id) on delete cascade,
  external_subscription_id text not null,
  status text not null
    check (status in ('trialing', 'active', 'past_due', 'paused', 'cancelled', 'expired')),
  product_key text not null,
  current_period_start timestamptz,
  current_period_end timestamptz,
  cancel_at_period_end boolean not null default false,
  provider_payload jsonb not null default '{}'::jsonb,
  updated_at timestamptz not null default now(),
  unique (customer_account_id, external_subscription_id)
);

create table billing.entitlements (
  id uuid primary key default gen_random_uuid(),
  user_id uuid not null references auth.users(id) on delete cascade,
  entitlement_key text not null,
  status text not null check (status in ('active', 'revoked', 'expired')),
  starts_at timestamptz not null default now(),
  ends_at timestamptz,
  source_subscription_id uuid references billing.subscriptions(id) on delete set null,
  created_at timestamptz not null default now(),
  unique (user_id, entitlement_key, starts_at),
  check (ends_at is null or ends_at > starts_at)
);

create index entitlements_lookup_idx
  on billing.entitlements (user_id, entitlement_key, status, ends_at);

create table billing.webhook_events (
  payment_provider text not null,
  external_event_id text not null,
  event_type text not null,
  payload jsonb not null,
  received_at timestamptz not null default now(),
  processed_at timestamptz,
  processing_error text,
  primary key (payment_provider, external_event_id)
);

create or replace function billing.has_active_entitlement(
  requested_user_id uuid,
  requested_entitlement text
)
returns boolean
language sql
stable
security definer
set search_path = pg_catalog, billing
as $$
  select exists (
    select 1
    from billing.entitlements e
    where e.user_id = requested_user_id
      and e.entitlement_key = requested_entitlement
      and e.status = 'active'
      and e.starts_at <= now()
      and (e.ends_at is null or e.ends_at > now())
  );
$$;

revoke all on function billing.has_active_entitlement(uuid, text) from public;
grant execute on function billing.has_active_entitlement(uuid, text) to anon, authenticated, service_role;

create or replace function billing.create_profile_for_new_user()
returns trigger
language plpgsql
security definer
set search_path = pg_catalog, billing
as $$
begin
  insert into billing.profiles (user_id, display_name)
  values (new.id, coalesce(new.raw_user_meta_data ->> 'display_name', new.email))
  on conflict (user_id) do nothing;
  return new;
end;
$$;

revoke all on function billing.create_profile_for_new_user() from public;

create trigger create_billing_profile_after_signup
  after insert on auth.users
  for each row execute function billing.create_profile_for_new_user();

create or replace function publishing.publish_release(target_release_id uuid)
returns void
language plpgsql
security definer
set search_path = pg_catalog, publishing, ingest, observability
as $$
declare
  target_status text;
begin
  select status
    into target_status
  from publishing.releases
  where id = target_release_id
  for update;

  if target_status is null then
    raise exception 'Release % does not exist', target_release_id;
  end if;

  if target_status <> 'staged' then
    raise exception 'Release % must be staged, found %', target_release_id, target_status;
  end if;

  if not exists (
    select 1 from publishing.release_sources where release_id = target_release_id
  ) then
    raise exception 'Release % has no declared sources', target_release_id;
  end if;

  if exists (
    select 1
    from publishing.release_sources rs
    join ingest.providers p using (provider_key)
    where rs.release_id = target_release_id
      and (
        p.permission_status <> 'approved'
        or p.commercial_serving_allowed is not true
        or p.permission_documented_at is null
      )
  ) then
    raise exception 'Release % includes a source that is not approved for commercial serving', target_release_id;
  end if;

  if exists (
    select 1
    from publishing.release_sources
    where release_id = target_release_id
      and row_count = 0
  ) then
    raise exception 'Release % includes an empty source', target_release_id;
  end if;

  if not exists (
    select 1 from publishing.release_games where release_id = target_release_id
  ) or not exists (
    select 1 from publishing.release_game_events where release_id = target_release_id
  ) then
    raise exception 'Release % is missing its game or event publication set', target_release_id;
  end if;

  if exists (
    select 1
    from observability.release_checks
    where release_id = target_release_id
      and status = 'fail'
  ) then
    raise exception 'Release % has failing validation checks', target_release_id;
  end if;

  if not exists (
    select 1
    from observability.release_checks
    where release_id = target_release_id
      and check_key = 'minimum_release_contract'
      and status = 'pass'
  ) then
    raise exception 'Release % has not passed minimum_release_contract', target_release_id;
  end if;

  perform 1
  from publishing.current_release
  where singleton = true
  for update;

  update publishing.releases r
  set status = 'superseded'
  from publishing.current_release c
  where c.singleton = true
    and r.id = c.release_id
    and r.id <> target_release_id
    and r.status = 'published';

  update publishing.releases
  set status = 'published', published_at = now()
  where id = target_release_id;

  update publishing.current_release
  set release_id = target_release_id, switched_at = now()
  where singleton = true;
end;
$$;

revoke all on function publishing.publish_release(uuid) from public, anon, authenticated;
grant execute on function publishing.publish_release(uuid) to service_role;

alter table ingest.providers enable row level security;
alter table observability.pipeline_runs enable row level security;
alter table ingest.batches enable row level security;
alter table ingest.raw_records enable row level security;
alter table canonical.teams enable row level security;
alter table canonical.people enable row level security;
alter table canonical.external_ids enable row level security;
alter table canonical.games enable row level security;
alter table canonical.game_events enable row level security;
alter table publishing.releases enable row level security;
alter table publishing.release_sources enable row level security;
alter table publishing.release_games enable row level security;
alter table publishing.release_game_events enable row level security;
alter table publishing.current_release enable row level security;
alter table observability.release_checks enable row level security;
alter table billing.profiles enable row level security;
alter table billing.customer_accounts enable row level security;
alter table billing.subscriptions enable row level security;
alter table billing.entitlements enable row level security;
alter table billing.webhook_events enable row level security;

create policy profiles_select_own
  on billing.profiles for select to authenticated
  using ((select auth.uid()) = user_id);

create policy profiles_update_own
  on billing.profiles for update to authenticated
  using ((select auth.uid()) = user_id)
  with check ((select auth.uid()) = user_id);

create policy entitlements_select_own
  on billing.entitlements for select to authenticated
  using ((select auth.uid()) = user_id);

create policy current_release_checks_read
  on observability.release_checks for select to anon, authenticated
  using (
    exists (
      select 1
      from publishing.current_release c
      where c.singleton = true and c.release_id = release_checks.release_id
    )
  );

create policy current_release_read
  on publishing.current_release for select to anon, authenticated
  using (true);

create policy releases_current_read
  on publishing.releases for select to anon, authenticated
  using (
    exists (
      select 1
      from publishing.current_release c
      where c.singleton = true and c.release_id = id
    )
  );

create policy providers_released_read
  on ingest.providers for select to anon, authenticated
  using (
    commercial_serving_allowed
    and exists (
      select 1
      from publishing.current_release c
      join publishing.release_sources rs on rs.release_id = c.release_id
      where c.singleton = true and rs.provider_key = providers.provider_key
    )
  );

create policy release_sources_current_read
  on publishing.release_sources for select to anon, authenticated
  using (
    exists (
      select 1
      from publishing.current_release c
      where c.singleton = true and c.release_id = release_sources.release_id
    )
  );

create policy release_games_audience_read
  on publishing.release_games for select to anon, authenticated
  using (
    exists (
      select 1
      from publishing.current_release c
      where c.singleton = true and c.release_id = release_games.release_id
    )
    and (
      audience = 'public'
      or (
        audience = 'member'
        and (select auth.uid()) is not null
        and billing.has_active_entitlement((select auth.uid()), 'member')
      )
    )
  );

create policy release_game_events_audience_read
  on publishing.release_game_events for select to anon, authenticated
  using (
    exists (
      select 1
      from publishing.current_release c
      where c.singleton = true and c.release_id = release_game_events.release_id
    )
    and (
      audience = 'public'
      or (
        audience = 'member'
        and (select auth.uid()) is not null
        and billing.has_active_entitlement((select auth.uid()), 'member')
      )
    )
  );

create policy canonical_games_released_read
  on canonical.games for select to anon, authenticated
  using (
    exists (
      select 1
      from publishing.current_release c
      join publishing.release_games rg on rg.release_id = c.release_id
      where c.singleton = true
        and rg.game_id = games.id
        and (
          rg.audience = 'public'
          or (
            rg.audience = 'member'
            and (select auth.uid()) is not null
            and billing.has_active_entitlement((select auth.uid()), 'member')
          )
        )
    )
  );

create policy canonical_events_released_read
  on canonical.game_events for select to anon, authenticated
  using (
    exists (
      select 1
      from publishing.current_release c
      join publishing.release_game_events re on re.release_id = c.release_id
      where c.singleton = true
        and re.game_event_id = game_events.id
        and (
          re.audience = 'public'
          or (
            re.audience = 'member'
            and (select auth.uid()) is not null
            and billing.has_active_entitlement((select auth.uid()), 'member')
          )
        )
    )
  );

grant select on ingest.providers to anon, authenticated;
grant select on canonical.games, canonical.game_events to anon, authenticated;
grant select on publishing.releases, publishing.release_sources,
  publishing.release_games, publishing.release_game_events,
  publishing.current_release to anon, authenticated;
grant select on billing.profiles, billing.entitlements to authenticated;
grant update (display_name, updated_at) on billing.profiles to authenticated;
grant select on observability.release_checks to anon, authenticated;
grant select, insert, update, delete on all tables in schema
  ingest, canonical, publishing, observability, billing to service_role;

create view api.public_games
with (security_invoker = true)
as
select
  g.id,
  g.played_on,
  g.game_number,
  g.home_team_code,
  g.away_team_code,
  g.site_code,
  g.day_night,
  p.provider_key as source,
  p.required_attribution as attribution,
  r.release_key,
  r.published_at
from publishing.current_release c
join publishing.releases r on r.id = c.release_id
join publishing.release_games rg
  on rg.release_id = r.id and rg.audience = 'public'
join canonical.games g on g.id = rg.game_id
join ingest.providers p on p.provider_key = g.source_provider_key
where c.singleton = true
  and p.commercial_serving_allowed;

create view api.public_game_events
with (security_invoker = true)
as
select
  e.id,
  e.game_id,
  e.event_sequence,
  e.inning,
  e.batting_side,
  e.batter_source_id,
  e.count_state,
  e.pitch_sequence,
  e.event_text,
  p.provider_key as source,
  p.required_attribution as attribution,
  r.release_key,
  r.published_at
from publishing.current_release c
join publishing.releases r on r.id = c.release_id
join publishing.release_game_events re
  on re.release_id = r.id and re.audience = 'public'
join canonical.game_events e on e.id = re.game_event_id
join ingest.providers p on p.provider_key = e.source_provider_key
where c.singleton = true
  and p.commercial_serving_allowed;

create view api.member_games
with (security_invoker = true)
as
select
  g.id,
  g.played_on,
  g.game_number,
  g.home_team_code,
  g.away_team_code,
  g.site_code,
  g.day_night,
  rg.audience,
  p.provider_key as source,
  p.required_attribution as attribution,
  r.release_key,
  r.published_at
from publishing.current_release c
join publishing.releases r on r.id = c.release_id
join publishing.release_games rg on rg.release_id = r.id
join canonical.games g on g.id = rg.game_id
join ingest.providers p on p.provider_key = g.source_provider_key
where c.singleton = true
  and p.commercial_serving_allowed;

create view api.member_game_events
with (security_invoker = true)
as
select
  e.id,
  e.game_id,
  e.event_sequence,
  e.inning,
  e.batting_side,
  e.batter_source_id,
  e.count_state,
  e.pitch_sequence,
  e.event_text,
  re.audience,
  p.provider_key as source,
  p.required_attribution as attribution,
  r.release_key,
  r.published_at
from publishing.current_release c
join publishing.releases r on r.id = c.release_id
join publishing.release_game_events re on re.release_id = r.id
join canonical.game_events e on e.id = re.game_event_id
join ingest.providers p on p.provider_key = e.source_provider_key
where c.singleton = true
  and p.commercial_serving_allowed;

create view api.release_status
with (security_invoker = true)
as
select
  r.release_key,
  r.published_at,
  min(rs.source_through) as source_through,
  sum(rs.row_count) as rows_published,
  count(*) as source_count
from publishing.current_release c
join publishing.releases r on r.id = c.release_id
join publishing.release_sources rs on rs.release_id = r.id
where c.singleton = true
group by r.release_key, r.published_at;

create view api.pipeline_health
with (security_invoker = true)
as
select
  r.release_key,
  max(rc.checked_at) as checked_at,
  count(*) filter (where rc.status = 'pass') as checks_passed,
  count(*) filter (where rc.status = 'warn') as checks_warning,
  count(*) filter (where rc.status = 'fail') as checks_failed,
  case
    when count(*) filter (where rc.status = 'fail') > 0 then 'fail'
    when count(*) filter (where rc.status = 'warn') > 0 then 'warn'
    else 'pass'
  end as status
from publishing.current_release c
join publishing.releases r on r.id = c.release_id
join observability.release_checks rc on rc.release_id = r.id
where c.singleton = true
group by r.release_key;

grant select on api.public_games, api.public_game_events, api.release_status,
  api.pipeline_health to anon, authenticated;
grant select on api.member_games, api.member_game_events to authenticated;

commit;
