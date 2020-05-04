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
(*cards = Import/@FileNames["Cards/*.png", NotebookDirectory[]];*)
(**)
(*BubbleSortAnimation::usage="Function to create the bubblesort animation"*)
(*BubbleSortGame::usage="Function to create the bubblesort game"*)
(**)
(*Begin["Private`"]*)
(**)
(*(*function to swap the elements of an array given two indices*)*)
(*Swap[array_, i_, j_] := ReplacePart[array, {i-> array[[j]], j-> array[[i]]}];*)
(**)
(*(*Function to get value of a card*)*)
(*CardToNumber[c_]:= Module[{card = c},*)
(*						index = Part[Position[cards, c],1]-1; (*index of the card*)*)
(*						Part[Mod[index,10],1]]*)
(**)
(*(*Function to get a card with a certain value*)*)
(*NumberToCard[num_]:= Module[{number = num},*)
(*							color = RandomInteger[{0,3}];  (*number expressing a random color*)*)
(*							Part[cards, num+1 + 10*color]]*)
(**)
(*(*Function to create a grid for the array*)*)
(*CreateArrayGrid[array_]:= Module[{a = array,i}, Table[Grid[{{Part[a, i]},{i-1}}],{i,1,Length[a]}]]*)
(**)
(*(*Function to create a grid for the array with the elements highlighted in red according to the indices passed in input*)*)
(*CreateArrayGrid[array_, highlight_, final_]:= Module[{a = array,i, h = highlight, f = final, list}, *)
(*										     (*create a table of grids with the elements and highlight them if needed*)*)
(*											list =Table[Grid[{{Part[a, i]},{i-1}},Frame -> True, *)
(*														If[MemberQ[h, i], *)
(*													FrameStyle->  Directive[Red,Thick], *)
(*													If[MemberQ[f,i], *)
(*														FrameStyle->  Directive[Black, Thick], *)
(*														FrameStyle->  Directive[White, Thick]]]],*)
(*													{i,1,Length[a]}];*)
(*											Grid[{list}]]*)
(**)
(*(*Function to compute the bubblesort frames with the associated comparisons*)*)
(*BubbleSortFrames[list_]:=Module[{l=list,i,j,len=Length[list]},*)
(*								steps = List[list, list];        (*list of steps*)*)
(*								comparisons = List[{}];           (*list of compared elements in each step*)*)
(*							   (*for n-1 times*)*)
(*								Do[*)
(*							       (*for n-1 times*)*)
(*								      Do[*)
(*								      (*compare adjacent elements and swap if  in wrong order*)*)
(*										If[CardToNumber[l[[j]]]> CardToNumber[l[[j+1]]],*)
(*									       l = Swap[l, j, j+1];*)
(*                                          (*update steps and comparisons list*)*)
(*									     comparisons = Append[comparisons, List[j, j+1]];*)
(*									     steps = Append[steps, l],*)
(*										   True];*)
(*								         (*update steps and comparisons list again*)*)
(*								      comparisons = Append[comparisons, List[j, j+1]];*)
(*								      steps = Append[steps, l],*)
(*									  {j,1,len-1}];,*)
(*									{i,1,len-1}]; *)
(*								comparisons = Append[comparisons, List[{}]];    (*add final array*)*)
(*								List[steps, comparisons]]*)
(**)
(*(*Function to compute the bubblesort frames with the associated comparisons*)*)
(*OptimizedBubbleSortFrames[list_]:=Module[{l=list,i,j,len=Length[list], n},*)
(*								steps = List[list];        (*list of steps*)*)
(*								comparisons = List[{}];           (*list of compared elements in each step*)*)
(*							     final = List[{}];*)
(*							     n = len;*)
(*							   (*for len-1 times*)*)
(*								Do[*)
(*							       (*for n-1 times*)*)
(*								      Do[*)
(*                                                                              (*update steps and comparisons list*)*)
(*								      comparisons = Append[comparisons, List[j, j+1]];*)
(*								      steps = Append[steps, l];*)
(*						                      final = Append[final, Last[final]];*)
(*								      (*compare adjacent elements and swap if  in wrong order*)*)
(*										If[CardToNumber[l[[j]]]> CardToNumber[l[[j+1]]],*)
(*									       l = Swap[l, j, j+1];*)
(*                                          (*update steps and comparisons list*)*)
(*									     comparisons = Append[comparisons, List[j, j+1]];*)
(*									     steps = Append[steps, l];*)
(*								         final = Append[final, Last[final]],*)
(*										   True],*)
(*									  {j,1,n-1}]; *)
(*                                                                               final = Append[final, Join[Last[final],List[n]]];*)
(*								     comparisons = Append[comparisons, {}];*)
(*								             steps = Append[steps, l];*)
(*								       n = n-1,								      *)
(*									{i,1,len-1}]; *)
(*							     steps = Append[steps, l];*)
(*								comparisons = Append[comparisons, {}];    (*add final array*)*)
(*							    final = Append[final, {}];*)
(*								List[steps, comparisons, final]]*)
(**)
(*(*Function to create the bubblesort animation*)*)
(*BubbleSortAnimation[length_, toggler_, optimized_] := Module[{len = length, opt = optimized, array, frames, steps, comparisons},*)
(*									array = Table[NumberToCard[RandomInteger[9]],len];  (*cards array*)*)
(*									If[opt,frames = OptimizedBubbleSortFrames[array]; final = Part[frames,3],*)
(*										frames = BubbleSortFrames[array]; final = List[]];*)
(*								     steps = Part[frames,1];                          (*steps of execution*)*)
(*									comparisons = Part[frames,2];            (*compared elements in each step*)*)
(*								    (*animate the execution*)*)
(*									Animate[*)
(*										CreateArrayGrid[Part[steps,toggler], Part[comparisons, toggler], Part[final, toggler]],*)
(*										{toggler,1,Length[steps],1}, *)
(*										ControlPlacement->Top, DefaultDuration -> Length[steps],*)
(*									         AnimationRepetitions->1, AnimationRunning->False]]*)
(**)
(*(*Function to create the bubblesort game*)*)
(*BubbleSortGame[length_] := DynamicModule[{len = length, originalArray, steps, currentArray, arrayDisplay, trial, swapNumber, enabled, i,j},*)
(*						   originalArray=Table[NumberToCard[RandomInteger[9]], len];     (*starting array*)*)
(*						   steps = Rest[DeleteDuplicates[Part[BubbleSortFrames[originalArray],1]]]; *)
(*					      (*list of different steps for the array*)*)
(*						   currentArray = originalArray;    (*current state of the array*)*)
(*						  arrayDisplay= CreateArrayGrid[currentArray];       (*grid with the current array*)*)
(*						  swapNumber = 0;      (*number of executed swaps*)*)
(*                           enabled = True;      (*used to disable the swap button at the end of the game*)*)
(*						  message = "";           (*info message*)*)
(*     *)
(*						(*togglerbar to see the current array and select elements*)*)
(*                                                               selectionBar = Dynamic[TogglerBar[Dynamic[selection],arrayDisplay]];            *)
(*						*)
(*						(*button to swap the selected elements*)*)
(*						swapButton = Button["Scambia",*)
(*										    (*if the user selected two elements*)*)
(*										    Dynamic[If[Length[selection]== 2, *)
(*													(*swap them updating the current array*)*)
(*                                                      trial = currentArray;*)
(*													  i = Part[selection, 1,1,2,1];  (*first index*)*)
(*													 j = Part[selection, 2,1,2,1];    (*second index*)*)
(*															trial = Swap[trial, i+1, j+1];*)
(*															selection = List[];     (*reset the selection*)*)
(*													(*if the move was correct*)*)
(*						                            If[swapNumber+1 <=Length[steps]&&trial == Part[steps,swapNumber+1], *)
(*							                           message = "Scambio giusto!"; *)
(*											          swapNumber = swapNumber +1;      (*update the number of swaps*)*)
(*											          (*display the new array*)*)
(*                                                       currentArray = trial;*)
(*											          arrayDisplay= CreateArrayGrid[currentArray],*)
(*											          (*if the user was wrong*) *)
(*							                             message = "Scambio sbagliato!"],*)
(*													   (*the user selected less or more than two elements*)*)
(*													   message = "Numero di elementi da scambiare non valido"; *)
(*													(*reset the selection*)*)
(*													selection = List[]]], *)
(*										    Enabled -> Dynamic[enabled]];*)
(**)
(*(*button that the user presses when they think they're done*)*)
(*finishButton = Button["Ho finito!", *)
(*					Dynamic[If[swapNumber == Length[steps],   (*check if they reached the final step*)*)
(*							enabled = False;    (*disable the swap button*)*)
(*							message = "Complimenti, l'array \[EGrave] ordinato!",*)
(*							message = "Non hai ancora finito..."]]];*)
(*(*display elements of the game*)*)
(*Grid[{{selectionBar}, {swapButton, finishButton}, {Dynamic[message]}}]];          *)
(**)
(*End[]*)
(**)
(*EndPackage[]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
