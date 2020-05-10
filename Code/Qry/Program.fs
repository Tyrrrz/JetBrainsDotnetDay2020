open Qry
open Qry.Query

let books = Books.getAll()

let input = """
filterby Category = 'Fantasy'
orderby Rating asc
take 1
"""

let result = parse input

match result with
| Result.Ok res ->
    let queryResult = execute res books
    List.iter (fun i -> printfn "%O" i) queryResult
| Result.Error err -> printfn "%O" err