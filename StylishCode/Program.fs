open System

type Delivery =
    | AsBilling
    | Physical of string
    | Download

type BillingDetails = {
    Name : string
    Billing : string
    Delivery : Delivery }

let myOrder = { 
    Name = "Kit Eason" 
    Billing = "112 Fibonacci Street\nErehwon\n35813" 
    Delivery = AsBilling }

let hisOrder = {
    Name = "John Doe" 
    Billing = "314 Pi Avenue\nErewhon\n15926" 
    Delivery = Physical "16 Planck Parkway\nErewhon\n62291" }

let herOrder = {
    Name = "Jane Smith"
    Billing = "9 Gravity Road\nErewhon\n80665"
    Delivery = Download } 

let tryDeliveryLabel (billingDetails : BillingDetails) =
    match billingDetails.Delivery with
        | AsBilling -> billingDetails.Billing |> Some
        | Physical address -> address |> Some
        | Download -> None
        |> Option.map (fun address -> sprintf "%s\n%s" billingDetails.Name address)

let deliveryLabels (billingDetails : BillingDetails seq) =
            billingDetails 
                |> Seq.choose tryDeliveryLabel

let tryLastLine (address : string) =
    let parts = address.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    match parts with
        | [||] -> None
        | parts -> parts |> Array.last |> Some

let tryPostalCode (codeString : string) =
    match Int32.TryParse(codeString) with
        | true, i -> i |> Some
        | false, _ -> None

let postalCodeHub (code : int) = if code = 62291 then "Hub 1" else "Hub 2"

let tryHub (billingDetails : BillingDetails) =
    billingDetails
        |> tryDeliveryLabel
        |> Option.bind tryLastLine
        |> Option.bind tryPostalCode
        |> Option.map postalCodeHub

[<EntryPoint>]
let main _ =

    // let milesYards = 1.1 |> MilesYards.fromMilesPointYards |> MilesYards.toDecimalMiles 
    // printfn "%A" milesYards

    myOrder |> tryHub |> ignore

    hisOrder |> tryHub |> ignore

    let orders = [| myOrder; hisOrder; herOrder |]

    orders |> deliveryLabels |> ignore

    let _key = System.Console.ReadKey()
    0