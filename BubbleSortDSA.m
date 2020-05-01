(* ::Package:: *)

(* ::Input:: *)
(*(* :Title:BestExample*)*)
(*(* :Context:BestExample`*)*)
(*(* :Author:GS*)*)
(*(* :Summary:an example of good programming style*)*)
(*(* :Copyright:GS 2019*)*)
(*(* :Package Version:1*)*)
(*(* :Mathematica Version:12.1*)*)
(*(* :History:*)*)
(*(* :Keywords:programming style,local variables*)*)
(*(* :Sources:biblio*)*)
(*(* :Discussion:*)*)
(**)
(*BeginPackage["BubbleSortDSA`"]*)
(**)
(*(*PowerSum::usage="PowerSum[x, n] returns the sum of the first n powers of x."*)*)
(*NumberToCard::usage=""*)
(*CardToNumber::usage=""*)
(**)
(*Begin["Private`"]*)
(**)
(*cards = Import/@FileNames["Cards/*.png",NotebookDirectory[]];*)
(**)
(*(*Function to get value of a card*)*)
(*CardToNumber[c_]:= Module[{card = c},index = Part[Position[cards, c],1]-1; Part[Mod[index,10],1]]*)
(**)
(*(*Function to get a card with a certain value*)*)
(*NumberToCard[num_]:= Module[{number = num},color = RandomInteger[{0,3}]; Part[cards, num+1 + 10*color]]*)
(**)
(*CreateArrayGrid[array_]:= Table[Grid[{{Part[array, i]},{i }}, Frame-> All],{i,1,Length[array]}]*)
(**)
(*BubbleSort[list_List]:=Module[{l=list,i,j,len=Length[list]},*)
(*							steps = List[list];*)
(*							Do[*)
(*								Do[*)
(*									If[CardToNumber[l[[j]]]> CardToNumber[l[[j+1]]],*)
(*									{l[[j]],l[[j+1]]}={l[[j+1]],l[[j]]};*)
(*									steps = Append[steps, l];,*)
(*									True],*)
(*								{j,1,len-1}];,*)
(*							{i,1,len-1}]; l]*)
(**)
(*array = Table[NumberToCard[RandomInteger[9]],4];*)
(*BubbleSort[array];*)
(*steps;*)
(*Manipulate[CreateArrayGrid[Part[steps,n]],{n,1,Length[steps],1}, ControlPlacement->Top]*)
(**)
(*Swap[array_, i_, j_] := ReplacePart[array, {i+1-> array[[j+1]], j+1-> array[[i+1]]}];*)
(*dim = 10;*)
(*(*originalArray=Table[Part[cards,RandomInteger[40]], dim];*)*)
(*originalArray=Table[NumberToCard[RandomInteger[9]], dim];*)
(*currentArray = originalArray;*)
(*arrayDisplay= CreateArrayGrid[currentArray];*)
(*Dynamic[TogglerBar[Dynamic[selection],arrayDisplay]]*)
(*Button["Scambia", Dynamic[If[Length[selection]== 2, *)
(*							currentArray = Swap[currentArray, Part[selection, 1,1,2,1], Part[selection, 2,1,2,1]];*)
(*							selection = List[];*)
(*							arrayDisplay= CreateArrayGrid[currentArray, dim],*)
(*							selection = List[]]]]*)
(**)
(*End[]*)
(**)
(*EndPackage[]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
