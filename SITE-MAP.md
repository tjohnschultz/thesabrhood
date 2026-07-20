# Planned finished SABRhood site

The public site is organized around daily discovery, deep research, interactive
analysis, and broadcaster products. Private production tools feed compact,
reviewable outputs into those public surfaces.

```mermaid
flowchart TD
  Home[Home / latest intelligence] --> Today[Today / signal desk]
  Home --> StoryDesk[Story Engine / assignment queue]
  Home --> Races[League races]
  Home --> Research[Research stories]
  Home --> Newsletter[Daily newsletter]
  Today --> Players[Player intelligence]
  Today --> Teams[Team intelligence]
  Today --> History[History and milestones]
  Today --> Matchups[Matchup edges]
  Players --> PitchLab[Pitch and sequence lab]
  Matchups --> PitchLab
  Teams --> TeamPages[Thirty team dossiers]
  Races --> Forecasts[Projections and simulations]
  Forecasts --> Decisions[Manager and bullpen decisions]
  Research --> StoryPages[Articles and visual spotlights]
  Newsletter --> Archive[Newsletter archive]
  Home --> Broadcast[Broadcast services]
  Broadcast --> PacketLibrary[Public packet portfolio]
  Broadcast --> PacketStudio[Private packet studio]
  PacketStudio --> HTML[Offline HTML packet]
  PacketStudio --> PDF[PDF packet]
  Home --> AnalyticsLab[Private analytics lab]
  AnalyticsLab --> PlayerSearch[Player and team search]
  AnalyticsLab --> ChartBuilder[Chart and spotlight builder]
  AnalyticsLab --> Downloads[Images, tables, and research exports]
  Data[Automated MLB data pipeline] --> Today
  Data --> Newsletter
  Data --> Forecasts
  Data --> PacketStudio
  Data --> AnalyticsLab
```

## Product boundaries

- Public editorial: home, Today, races, players, teams, history, Pitch Lab,
  Matchup Edges, Story Engine, projections, newsletter, research, and selected
  packet examples.
- Private production: analytics lab, packet studio, editorial queue, and data
  quality controls.
- Reusable engine: `sabrhoodR`, derived-data jobs, simulations, historical
  context, manager models, and automated publishing workflows.
