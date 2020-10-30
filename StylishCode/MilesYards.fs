module MilesYards

open System

let private (~~) = float

type MilesYards = private | MilesYards of wholeMiles: int * yards: int

let fromMilesPointYards (milesPointYards: float): MilesYards =
    if milesPointYards < 0.0 then
        raise
        <| ArgumentOutOfRangeException("milesPointYards", "Must be > 0.0")

    let wholeMiles = milesPointYards |> floor |> int

    let fraction = milesPointYards - float (wholeMiles)

    if fraction > 0.1759 then
        raise
        <| ArgumentOutOfRangeException("milesPointYards", "Fractional part must be <= 0.1759")

    let yards = fraction * 10_000. |> round |> int

    MilesYards(wholeMiles, yards)

let toDecimalMiles (MilesYards (wholeMiles, yards)): float = ~~wholeMiles + (~~yards / 1760.)
