namespace DPRCalc

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Notation

[<JavaScript>]
module Client =    
    let [<Literal>] TemplateHtmlPath = __SOURCE_DIRECTORY__ + "/index.html"

    type IndexTemplate = Templating.Template<TemplateHtmlPath> 

    let People =
        ListModel.FromSeq [
            
            ]
    let (|PrimesLessThan|_|) n v =
        match v with
        | 11 -> [7; 5; 3; 2] |> Seq.take n |> Some
        | 7 -> [5; 3; 2] |> Seq.take n |> Some
        | 5 -> [3; 2] |> Seq.take n |> Some
        | 3 -> [2] |> Seq.take n |> Some
        | 2 -> [] |> Seq.take 0 |> Some
        | _ -> None

    let p n =
        match n with
        | PrimesLessThan 2 v -> 
            let v = v |> Array.ofSeq
            sprintf "%d %d" v.[0] v.[1]
        | _ -> sprintf "Nothing"
    let getInt (input : string) = System.Int32.Parse input

    let Main =
        JQuery.Of("#main").Empty().Ignore

        let AC = Var.Create ""
        let ToHit = Var.Create ""
        let Dice = Var.Create ""
        let Size = Var.Create ""
        let Plus = Var.Create ""
        let N = Var.Create ""
        
        IndexTemplate.Main.Doc(
            ListContainer =
                (ListModel.View People |> Doc.Convert (fun name ->
                    IndexTemplate.ListItem.Doc(DPR = View.Const name))
                ),
            AC = AC,
            ToHit = ToHit,
            Dice = Dice,
            Size = Size,
            Plus = Plus,
            N = N,
            Compute = (fun e ->
                let n, tohit, dice, size, plus, ac = getInt N.Value, getInt ToHit.Value, getInt Dice.Value, getInt Size.Value, getInt Plus.Value, getInt AC.Value
                let hitRate = match (21 - ac) + tohit with
                                | x when x >= 20 -> 0.95
                                | x when x <= 1 -> 0.05
                                | x -> (float x) / 20.
                // dpr = regular hit percentage * damage plus 
                let regularDpr = (float n) * hitRate * ((float (size + 1) * float dice) / 2. + float plus)
                let critDpr = (((float (size * 2) * float dice) / 2.) / 20.)
                let dpr = regularDpr + critDpr
                People.Add(sprintf "%dx +%d for %dd%d+%d vs AC %d = %f DPR" n tohit dice size plus ac dpr)
                
                )
        )
        |> Doc.RunById "main"

