open System

// All ordered picks {x_i1, x_i2, .. , x_ik} of k out of n elements {x_1,..,x_n}
// where i1 < i2 < .. < ik
module Perm =
  let picks n L = 
      let rec aux nleft acc L = seq {
          match nleft,L with
          | 0,_ -> yield acc
          | _,[] -> ()
          | nleft,h::t -> yield! aux (nleft-1) (h::acc) t
                          yield! aux nleft acc t }
      aux n [] L

  // Distribute an element y over a list:
  // {x1,..,xn} --> {y,x1,..,xn}, {x1,y,x2,..,xn}, .. , {x1,..,xn,y}
  let distrib y L =
      let rec aux pre post = seq {
          match post with
          | [] -> yield (L @ [y])
          | h::t -> yield (pre @ y::post)
                    yield! aux (pre @ [h]) t }
      aux [] L

  // All permutations of a single list = the head of a list distributed
  // over all permutations of its tail
  let rec getAllPerms = function
      | [] -> Seq.singleton []
      | h::t -> getAllPerms t |> Seq.collect (distrib h)

  // All k-element permutations out of n elements = 
  // all permutations of all ordered picks of length k combined
  let getPerms2 n lst = picks n lst |> Seq.collect getAllPerms

  // Generates the cartesian outer product of a list of sequences LL
  let rec outerProduct = function
      | [] -> Seq.singleton []
      | L::Ls -> L |> Seq.collect (fun x -> 
                  outerProduct Ls |> Seq.map (fun L -> x::L))

  // Generates all n-element combination from a list L
  let getPermsWithRep2 n L = 
      List.replicate n L |> outerProduct

///////////////////////////////////////////////////////////////////////////////

type Stud = Free | Occupied

type Top = Stud * Stud * Stud * Stud
type Bottom = Stud * Stud * Stud * Stud

type Brick = Top * Bottom

type Position = int * int * int

type TakenPositions = Set<Position>

let x = 1

[<EntryPoint>]
let main args =
  printfn "Arguments passed to program : %A" args

  let model = args |> Seq.map int

  printfn "x = %A" model

  // done
  0
