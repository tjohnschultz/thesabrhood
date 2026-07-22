# sabrhoodR

Private R package for the shared analytical engine behind The SABRhood.

The first migrated product is the starter-matchup Monte Carlo engine. The
package now also turns BaseballR play-by-play into stable pitch, plate
appearance, batted-ball, and pitcher-appearance views; classifies pitcher
roles; estimates recent bullpen availability; records manager pitcher-hook
decisions; estimates RE24 and run value; describes pitch sequences; produces
hitter, pitcher, pitch-type, platoon, and recent-form summaries; and generates
scored Lahman anniversary notes. Raw play-by-play is intentionally outside the
package.

## Package boundary

The package owns reusable data transformations, metrics, simulations,
visualizations, editorial selection, validation, and report data objects.
Quarto navigation, Shiny user interfaces, deployment workflows, credentials,
and raw data storage belong to consuming projects.

## Development status

This is an initial development package (`0.0.0.9000`). The simulation and data
contract APIs are the first canonical modules. The next implementation target
is a time-aware manager hook and replacement selector that can drive relief
pitcher entry in the Monte Carlo engine. Summary, trend, visualization, packet,
and newsletter functions will be migrated after duplicate definitions and
source-order overrides are resolved.
