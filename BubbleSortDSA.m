(* ::Package:: *)

(* :Title:BubbleSortDSA*)
(* :Context:BubbleSortDSA`*)
(* :Author:PaolaPersico,CarmeloCaruso,MelaniaGhelli,IlariaRinaldi*)
(* :Summary:an interactive Bubblesort lesson for DSA students*)
(* :Copyright:*)
(* :Package Version:1*)
(* :Mathematica Version:12.1*)
(* :History:*)
(* :Keywords:bubblesort,dsa*)
(* :Sources:biblio*)
(* :Discussion:*)

BeginPackage["BubbleSortDSA`"]

cards = Import/@FileNames["Cards/*.png", NotebookDirectory[]];

BubbleSortAnimation::usage="Function to create the bubblesort animation"
StartBubbleSortGame::usage="Function to start the bubblesort game"

Begin["Private`"]

(*function to swap the elements of an array given two indices*)
Swap[array_, i_, j_] := ReplacePart[array, {i-> array[[j]], j-> array[[i]]}];

(*Function to get value of a card*)
CardToNumber[c_]:= 
	Module[{card = c},
	index = Part[Position[cards, c],1]-1; (*index of the card*)
	Part[Mod[index,10],1]];

(*Function to get a card with a certain value*)
NumberToCard[num_]:= 
	Module[{number = num},
	color = RandomInteger[{0,3}];  (*number expressing a random color*)
	Part[cards, num+1 + 10*color]];

(*Function to create a grid for the array*)
CreateArrayGrid[array_]:= 
	Module[{a = array,i}, 
	Table[Grid[{{Part[a, i]},{i-1}}],{i,1,Length[a]}]];

(*Function to create a grid for the array with the elements highlighted in red according to the indices passed in input*)
CreateArrayGrid[array_, highlight_, final_]:= 
	Module[{a = array,i, h = highlight, f = final, list}, 
	(*create a table of grids with the elements and highlight them if needed*)
	list = Table[Grid[{{Part[a, i]},{i-1}},Frame -> True,
			(*highlight in red compared elements*)  
			If[MemberQ[h, i], FrameStyle->  Directive[Red,Thick], 
				(*highlight in black final elements*) 
				If[MemberQ[f,i], FrameStyle->  Directive[Black, Thick], 
					FrameStyle->  Directive[White, Thick]]]],
			{i,1,Length[a]}];
	Grid[{list}]];

(*Function to update the bubblesort steps, comparisons and final elements*)
AppendFrame[frames_, newStep_, newComparisons_, newFinal_] := 
	Module[{fr = frames, step = newStep, comp = newComparisons, fin = newFinal, steps, comparisons, final},
	steps = Append[Part[fr,1], step];
	comparisons= Append[Part[fr,2], comp];
	final = Append[Part[fr,3], fin];
	List[steps, comparisons, final]];

(*Function to compute the bubblesort frames with the associated comparisons and final elements*)
BubbleSortFrames[list_, optimized_]:=
	Module[{l = list, opt = optimized, i, j, len = Length[list], n, frames, final},
	(*first element: list of cards, second element: compared indices, third element: final elements*)
	frames = List[List[list],List[{}], List[{}]];
	n = len;
	(*for len-1 times*)
	Do[
		(*for n-1 times*)
		Do[
			(*update frames*)
			If[opt, final = Last[Part[frames,3]], final ={}]; 
			frames = AppendFrame[frames, l, List[j, j+1], final];
			(*compare adjacent elements and swap if  in wrong order*)
			If[CardToNumber[l[[j]]]> CardToNumber[l[[j+1]]], 
				l = Swap[l, j, j+1];
				(*update frames*)
				frames = AppendFrame[frames, l, Last[Part[frames,2]], final],True],
		{j,1,n-1}]; 
		(*if optimized bubblesort, add final elements and consider one less element to compare*)
		If[opt, frames = AppendFrame[frames, l, {}, Join[Last[Part[frames,3]], List[n]]]; n = n-1, True],
	{i,1,len-1}]; 
	(*update frames*)
	AppendFrame[frames, l, {}, Range[1,len]]];

(*Function to create the bubblesort animation*)
BubbleSortAnimation[length_, toggler_, optimized_:False] := 
	Module[{len = length, opt = optimized, array, frames, steps, comparisons, final},
	array = Table[NumberToCard[RandomInteger[9]],len];  (*cards array*)
	frames = BubbleSortFrames[array, opt];
	steps = Part[frames,1];                          (*steps of execution*)
	comparisons = Part[frames,2];            (*compared elements in each step*)
	final = Part[frames,3];
	(*animate the execution*)
	Animate[
		CreateArrayGrid[Part[steps,toggler], Part[comparisons, toggler], Part[final, toggler]],{toggler,1,Length[steps],1}, 
		ControlPlacement->Top, DefaultDuration -> Length[steps],AnimationRepetitions->1, AnimationRunning->False]];

(*Function to start the bubblesort game*)
StartBubbleSortGame[optimized_:False] := 
	DynamicModule[{opt = optimized, inputArray = Null, len = 6, start, message = "", game = ""},
	(*radiobuttonbar to select the length of the array*)
	lengthBar = RadioButtonBar[Dynamic[len],Range[1,6]];
	(*array in input*)
	elements = InputField[Dynamic[inputArray], FieldHint-> "{0,1,2,3,4,5}"]; 
	(*button to create the cards array and start the game*)    
	createButton = Button["Crea nuovo array", 
	If[SameQ[inputArray, Null], (*if the user hasn't specified an array*)
		(*a random array with the selected length is created*)
		start = Table[NumberToCard[RandomInteger[9]], len  ]; 
		game = BubbleSortGame[start]; 
		inputArray = Null; 
		message = "",
		(*otherwise we check if the input is a list of integers between 0 and 9*)
		If[MatchQ[inputArray, { (0|1|2|3|4|5|6|7|8|9)..}], 
			start = Map[NumberToCard,inputArray ];
			(*if the length of the array is the same as the one selected, start the game*)
			If[Length[start] == len, 
		game = BubbleSortGame[start, opt]; inputArray = Null; message = "", 
		 message = "Lunghezza array sbagliata"], 
			message = "L'array pu\[OGrave] contenere solo interi da 0 a 9"]]];

	(*return the graphics elements*)
	inputGrid = Grid[{{"lunghezza dell'array:", lengthBar}, {"array:", elements},{createButton}}, Alignment->Left];
  Grid[{{inputGrid }, {Dynamic[Style[message, Red]]}, {}, {Dynamic[game]} }, Alignment->Left] ];

(*Function that takes an array as input and implements the bubblesort game*)
BubbleSortGame[start_, optimized_:False] := 
	DynamicModule[{originalArray = start, opt = optimized, steps, currentArray, arrayDisplay, trial, swapNumber = 0, enabled = True, i, j},
	steps = Rest[DeleteDuplicates[Part[BubbleSortFrames[originalArray, opt],1]]]; 
	(*list of different steps for the array*)
	currentArray = originalArray;    (*current state of the array*)
	arrayDisplay= CreateArrayGrid[currentArray];       (*grid with the current array*)
	message = "";           (*info message*)
	successMessage = "";

	(*togglerbar to see the current array and select elements*)
	selectionBar = Dynamic[TogglerBar[Dynamic[selection],arrayDisplay]];            

	(*button to swap the selected elements*)
	swapButton = Button["Scambia",
					(*if the user selected two elements*)
					Dynamic[If[Length[selection]== 2, 
							(*swap them updating the current array*)
							trial = currentArray;
							i = Part[selection, 1,1,2,1];  (*first index*)
							j = Part[selection, 2,1,2,1];    (*second index*)
							trial = Swap[trial, i+1, j+1];
							selection = List[];     (*reset the selection*)
							(*if the move was correct*)
							If[swapNumber+1 <=Length[steps] && trial == Part[steps,swapNumber+1], 
								message = "";
								successMessage = "Scambio giusto!"; 
								swapNumber = swapNumber +1;      (*update the number of swaps*)
								(*display the new array*)
								currentArray = trial;
								arrayDisplay= CreateArrayGrid[currentArray],
								(*if the user was wrong*) 
								successMessage = "";
								message = "Scambio sbagliato!"],
								(*the user selected less or more than two elements*)
								successMessage = "";
								message = "Numero di elementi da scambiare non valido"; 
								(*reset the selection*)
								selection = List[]]], 
					Enabled -> Dynamic[enabled]];

	(*button that the user presses when they think they're done*)
	finishButton = Button["Ho finito!", 
					Dynamic[If[swapNumber == Length[steps],   (*check if they reached the final step*)
								enabled = False;    (*disable the swap button*)
								message = "";
								successMessage = "Complimenti, l'array \[EGrave] ordinato! \[HappySmiley]",
								message = "Non hai ancora finito..."]]];

	(*display elements of the game*)
	gameGrid = Dynamic[Grid[{{selectionBar}, {swapButton, finishButton}}, Alignment->Left]];
	Dynamic[Grid[{{gameGrid}, {Dynamic[Style[message, Red]]}, {Dynamic[Style[successMessage, Bold, Darker[Green]]]}}, Alignment->Left]]];  
        

End[]

EndPackage[]


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
