namespace StockPortfolioManager

open WebSharper
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating

module Main =

    type Stock = {
        Symbol: string
        Quantity: int
        Price: Var<float>
    }

    let stocks = ListModel<Stock>(fun s -> s.Symbol)

    let addStock symbol quantity price =
        let stock = { Symbol = symbol; Quantity = quantity; Price = Var.Create price }
        stocks.Add stock

    let removeStock symbol =
        stocks.RemoveByKey symbol

    let fetchStockPrice symbol =
        // Simulate fetching stock price (replace with real API call)
        async {
            do! Async.Sleep 1000 // Simulate network delay
            return System.Random().NextDouble() * 100.0 // Random price
        }

    let updateStockPrices () =
        async {
            for stock in stocks.Value do
                let! price = fetchStockPrice stock.Symbol
                stock.Price.Value <- price
        }
        |> Async.Start

    [<SPAEntryPoint>]
    let Main () =
        let symbol = Var.Create ""
        let quantity = Var.Create 1

        let add () =
            let sym = symbol.Value.ToUpper()
            if not (stocks.ContainsKey sym) then
                async {
                    let! price = fetchStockPrice sym
                    addStock sym quantity.Value price
                }
                |> Async.Start
            symbol.Value <- ""
            quantity.Value <- 1

        let remove sym = removeStock sym

        updateStockPrices ()
        async {
            while true do
                do! Async.Sleep 5000
                updateStockPrices ()
        }
        |> Async.Start

        Doc.Concat [
            div [] [
                h2 [] [text "Stock Portfolio Manager"]
                div [] [
                    input [attr.placeholder "Stock Symbol"; bind.value symbol]
                    input [attr.placeholder "Quantity"; bind.valueAsInt quantity]
                    button [on.click (fun _ -> add())] [text "Add Stock"]
                ]
                div [] [
                    h3 [] [text "Your Portfolio"]
                    div [] [
                        Doc.BindSeq (fun stock ->
                            div [] [
                                text (sprintf "%s: %d shares at $%.2f each" stock.Symbol stock.Quantity stock.Price.Value)
                                button [on.click (fun _ -> remove stock.Symbol)] [text "Remove"]
                            ]) stocks.View
                    ]
                ]
                div [] [
                    h3 [] [text "Portfolio Metrics"]
                    div [] [
                        Doc.BindView (fun stocks ->
                            let totalValue = stocks |> List.sumBy (fun s -> s.Quantity * s.Price.Value)
                            div [] [text (sprintf "Total Portfolio Value: $%.2f" totalValue)]
                        ) stocks.View
                    ]
                ]
            ]
        ]
        |> Doc.RunById "main"
