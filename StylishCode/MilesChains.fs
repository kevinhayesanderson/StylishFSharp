module MilesChains

open System

type MilesChains = private | MilesChains of wholeMiles: int * chains: int

let fromMilesChains (wholeMiles: int, chains: int) =
    if wholeMiles < 0 then
        raise
        <| ArgumentOutOfRangeException("wholeMiles", "Must be >= 0")

    if chains < 0 || chains >= 80 then
        raise
        <| ArgumentOutOfRangeException("chains", "Must be >= 0 and < 80")

    MilesChains(wholeMiles, chains)


let toDecimalMiles (MilesChains (wholeMiles, chains)): float =
    (float wholeMiles) + ((float chains) / 80.)
