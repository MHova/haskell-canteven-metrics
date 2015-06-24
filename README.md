# canteven-metrics

Provides a way to easily setup metrics using EKG.

It uses [canteven-config](https://github.com/SumAll/haskell-canteven-config) to
obtain a configuration used to setup [ekg](https://github.com/tibbe/ekg) server
and [ekg-carbon](https://github.com/ocharles/ekg-carbon).

## Usage:

```haskell
import qualified Canteven.Metrics as I.Canteven (setupMetrics)

main =
  I.Canteven.setupMetrics
```
