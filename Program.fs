// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

//
// F# program to analyze Divvy daily ride data.


#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [15,22,141,17,5,1124]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), starting day of week (0 Sunday-6 Saturday)
// and trip duration (secs), 
//
let rec getIndex index L =
  match index with 
  | 0 -> List.head L
  | _ -> getIndex (index - 1) (List.tail L)

let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

let rec getBikeIds (ridedata : List<List<int>>) =
  match ridedata with
  | [] -> []
  | hd::[] -> [(getIndex 2 hd)]
  | hd::tail -> (getIndex 2 hd)::getBikeIds tail

let rec checkDup num list =
  match list with
  | [] -> false
  | hd::tail -> if hd = num then true else checkDup num tail

let rec getUniqueIds bikeIds =
  match bikeIds with
  | [] -> []
  |hd::tail -> if (checkDup hd tail) then getUniqueIds tail
               else hd::getUniqueIds tail

let rec getNumRides bikeID (ridedata : List<List<int>>) = 
  match ridedata with
  | [] -> 0
  | hd::tail -> if (getIndex 2 hd) = bikeID then 1 + getNumRides bikeID tail else getNumRides bikeID tail

let rec getTotalSecs bikeID (ridedata : List<List<int>>)=
  match ridedata with
  | [] -> 0
  | hd::tail -> if (getIndex 2 hd) = bikeID then (getIndex 5 hd) + getTotalSecs bikeID tail else getTotalSecs bikeID tail

let rec getNumRidesToStation stationID (ridedata : List<List<int>>) =
  match ridedata with
  | [] -> 0
  | hd::tail -> if (getIndex 1 hd) = stationID then 1 + getNumRidesToStation stationID tail else getNumRidesToStation stationID tail

let rec getAvgTimeSpentLeadStation stationID (ridedata : List<List<int>>) =
  match ridedata with
  | [] -> []
  | hd::tail -> if (getIndex 1 hd) = stationID then float((getIndex 5 hd))::getAvgTimeSpentLeadStation stationID tail 
                            else getAvgTimeSpentLeadStation stationID tail 

let rec getNumTripDay day (ridedata : List<List<int>>) =
  match ridedata with
  | [] -> 0
  | hd::tail -> if (getIndex 4 hd) = day then 1 + getNumTripDay day tail
                else getNumTripDay day tail

let rec printStars num =
  match num with
  | 0 -> ()
  | 1 -> printf "*"
  | _ -> printf "*" 
         printStars (num - 1) 

let rec checkDupStations stationID (list : List<List<int>>) =
  match list with
  | [] -> false
  | hd::tail -> if (getIndex 1 hd) = stationID then true else checkDupStations stationID tail

let rec getUniqueDestinationStations (ridedata : List<List<int>>)=
  match ridedata with
  | [] -> []
  | hd::tail -> if (checkDupStations (getIndex 1 hd) tail) then getUniqueDestinationStations tail
                else (getIndex 1 hd)::getUniqueDestinationStations tail

let rec _count num (ridedata : List<list<int>>) =
  match ridedata with 
  | [] -> 0
  | hd::tail -> if num = (getIndex 1 hd) then 1 + _count num tail else _count num tail

let rec getStationsAndRides (uniqueDestinationStations : List<int>) (ridedata : List<List<int>>) =
  match uniqueDestinationStations with
  | [] -> []
  | hd::tail -> [hd ; (_count hd ridedata)]::getStationsAndRides tail ridedata

let rec print num sortedStationsAndRides = 
  match num with
  | 0 -> ()
  | _ -> printfn "# of rides to station %A: %A" (getIndex 0 (List.head sortedStationsAndRides)) (getIndex 1 (List.head sortedStationsAndRides)) 
         print (num - 1) (List.tail sortedStationsAndRides)
         
[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  //printfn "%A" ridedata
  
  let N = List.length ridedata
  let bikeIds = getBikeIds ridedata
  let uniqueBikeIds = getUniqueIds bikeIds 
  let M = List.length bikeIds

  printfn ""
  printfn "# of rides: %A" N
  printfn ""
  printfn "# of bikes: %A" (List.length uniqueBikeIds)
  printfn ""
  

  printf "BikeID> "
  let bikeID = System.Int32.Parse(System.Console.ReadLine())

  let numRides = getNumRides bikeID ridedata
  printfn ""
  printfn "# of rides for BikeID %d: %A" bikeID numRides
  printfn ""

  //let countBikeId = get_Specific_BikeID bikeID ridedata
  let time = getTotalSecs bikeID ridedata
  let minutes = time / 60
  let seconds = time - (minutes * 60)
  printfn "Total time spent riding BikeID %d: %d minutes %d seconds" bikeID minutes seconds 

  let avgSeconds = float(time) / float(numRides)
  printfn ""
  printfn "Average time spent riding BikeID %A: %.2f seconds" bikeID avgSeconds
  printfn ""

  printf "StationID> "
  let stationID = System.Int32.Parse(System.Console.ReadLine())
  let numRidesToStation = getNumRidesToStation stationID ridedata
  printfn ""
  printfn "# of rides to StationID %A: %A" stationID numRidesToStation
  printfn ""

  let timeSpentLeadStation = getAvgTimeSpentLeadStation stationID ridedata
  let avgTimeSpentLeadStation = List.average(timeSpentLeadStation)
  printfn "Average time spent on trips leading to StationID %A: %.2f seconds" stationID avgTimeSpentLeadStation

  printfn ""
  printf"Number of Trips on Sunday: %A" (getNumTripDay 0 ridedata)
  printfn ""
  printf"Number of Trips on Monday: %A" (getNumTripDay 1 ridedata)
  printfn ""
  printf"Number of Trips on Tuesday: %A" (getNumTripDay 2 ridedata)
  printfn ""
  printf"Number of Trips on Wednesday: %A" (getNumTripDay 3 ridedata)
  printfn ""
  printf"Number of Trips on Thursday: %A" (getNumTripDay 4 ridedata)
  printfn ""
  printf"Number of Trips on Friday: %A" (getNumTripDay 5 ridedata)
  printfn ""
  printf"Number of Trips on Saturday: %A" (getNumTripDay 6 ridedata)
  printfn ""
  printfn ""

  printf "0: "
  printStars ((getNumTripDay 0 ridedata) / 10)
  printfn " %A" (getNumTripDay 0 ridedata)
  printf "1: "
  printStars ((getNumTripDay 1 ridedata) / 10)
  printfn " %A" (getNumTripDay 1 ridedata)
  printf "2: "
  printStars ((getNumTripDay 2 ridedata) /10)
  printfn " %A" (getNumTripDay 2 ridedata)
  printf "3: "
  printStars ((getNumTripDay 3 ridedata) /10)
  printfn " %A" (getNumTripDay 3 ridedata)
  printf "4: "
  printStars ((getNumTripDay 4 ridedata) /10)
  printfn " %A" (getNumTripDay 4 ridedata)
  printf "5: "
  printStars ((getNumTripDay 5 ridedata) /10)
  printfn " %A" (getNumTripDay 5 ridedata)
  printf "6: "
  printStars ((getNumTripDay 6 ridedata) /10)
  printfn " %A" (getNumTripDay 6 ridedata)
  printfn ""

  let uniqueDestinationStations = List.sort(getUniqueDestinationStations ridedata)
  let stationsAndRides = getStationsAndRides uniqueDestinationStations ridedata 
  
  // printfn "unique destination stations: %A" (List.length uniqueDestinationStations)
  // printfn "%A" stationsAndRides

  // let testList = [0 .. 10]
  // printfn "%A" (getIndex 9 testList)
  let F elem =
    List.head (List.tail(elem))
  
  let sortedStationsAndRides = List.sortBy (fun elem -> - F elem) stationsAndRides

  print 10 sortedStationsAndRides
  printfn ""
  0 

