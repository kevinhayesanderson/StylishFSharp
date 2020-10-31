module Houses

    type House = { Address : string; Price : decimal }

    type PriceBand = | Cheap | Medium | Expensive

    let random = System.Random(Seed = 1)

    let getHouses count =
        Array.init count (fun i ->
            {
                Address = sprintf "%i Stochastic Street" (i+1)
                Price = random.Next(50_000, 500_000) |> decimal
            }
        )

    let trySchoolDistance (house : House) =
        let dist = random.Next(10) |> double
        if dist < 8. then Some dist
        else None

    let priceBand (price : decimal) =
        if price < 100_000m then Cheap
        else if price < 200_000m then Medium
        else Expensive

    let housePrices count =
        getHouses count
            |> Array.map (fun h -> sprintf "Address: %s - Price: %f" h.Address h.Price)

    let housesByBand count=
        getHouses count
            |> Array.groupBy (fun h -> priceBand h.Price)
            |> Array.map (fun (band, houses) ->
                    band, houses |> Array.sortBy (fun h -> h.Price))

    module Array =
        let inline tryAverageBy f (a : 'T[]) =
            if a.Length = 0 then None
            else a |> Array.averageBy f |> Some

    let averageOver count over =
        getHouses count
            |> Array.filter (fun h -> h.Price > over)
            |> Array.tryAverageBy (fun h -> h.Price)