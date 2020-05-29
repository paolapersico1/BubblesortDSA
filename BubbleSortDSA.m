(* ::Package:: *)

(* :Title:BubbleSortDSA*)
(* :Context:BubbleSortDSA`*)
(* :Author:PaolaPersico,CarmeloCaruso,MelaniaGhelli,IlariaRinaldi*)
(* :Summary:an interactive Bubblesort lesson for DSA students*)
(* :Copyright:Licenza Creative Commons Attribuzione 4.0 Internazionale*)
(* :Package Version:1*)
(* :Mathematica Version:12.1*)
(* :Keywords:bubblesort,dsa*)

BeginPackage["BubbleSortDSA`"]

cards = Import/@FileNames["Assets/Images/Cards/*.png", NotebookDirectory[]];
startIcon = Import[StringJoin[NotebookDirectory[],"Assets/Images/project-management.png"]];


PrintStartButton::usage="Function that prints start button"
BubbleSortAnimation::usage="Function to create the bubblesort animation. Input: length of array, toggler name, boolean optimized version"
StartBubbleSortExercise::usage="Function to start a bubblesort exercise. Input: a string with the type of exercise (Game or Quiz)"
PlotOptimizedBubbleSort::usage="Function to plot the Bubble Sort complexity compared to its optimized version. Input: toggler name"
PlotBubbleSort::usage="Function to plot the Bubble Sort complexity compared to x squared. Input: toggler name"	

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

(*Function to count the number of comparisons of Bubble Sort*)	
BubbleSortComparisons[length_, optimized_:False]:=
	Module[{len = length, opt = optimized,  n, count = 0},
	n = len;
	(*for len-1 times*)
	Do[
		(*for n-1 times*)
		Do[
			count=count+1,
		{j,1,n-1}]; 
		(*if optimized bubblesort, consider one less element to compare*)
		If[opt, n = n-1, True],
	{i,1,len-1}] ;
	count];

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
	Module[{l = list, opt = optimized, len = Length[list], n, frames, final},
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
StartBubbleSortExercise[type_:"Quiz"] := 
	DynamicModule[{inputArray = Null, len = 4, start, message = "", exercise = ""},
	(*radiobuttonbar to select the length of the array*)
	lengthBar = RadioButtonBar[Dynamic[len],Range[1,6]];
	(*array in input*)
	elements = InputField[Dynamic[inputArray], String, FieldHint-> "0,1,2,3,4,5"]; 
	(*button to create the cards array and start the game*)    
	createButton = Button["Crea nuovo vettore", 
	If[SameQ[inputArray, Null], (*if the user hasn't specified an array*)
		(*a random array with the selected length is created*)
		start = Table[NumberToCard[RandomInteger[9]], len]; 
		If[type == "Game", exercise = BubbleSortGame[start], exercise = BubbleSortQuiz[start]]; 
		inputArray = Null; 
		message = "",
		(*otherwise we check if the input is a list of integers between 0 and 9*)
		start = ToExpression[StringJoin["{", inputArray, "}"]];
		If[MatchQ[start, {(0|1|2|3|4|5|6|7|8|9)..}], 
			start = Map[NumberToCard, start];
			(*if the length of the array is the same as the one selected, start the game*)
			If[Length[start] == len, 
				If[type == "Game", exercise = BubbleSortGame[start], exercise = BubbleSortQuiz[start]]; 
				inputArray = Null; message = "", 
		        message = "Numero di elementi del vettore non corrispondente"], 
			message = "Il vettore pu\[OGrave] contenere solo interi tra 0 e 9 separati da virgole"]]];

	(*return the graphics elements*)
	inputGrid = Grid[{{"numero di elementi del vettore:", lengthBar}, {"vettore:", elements},{createButton}}, Alignment->Left];
  Grid[{{inputGrid }, {Dynamic[Style[message, Red]]}, {}, {Dynamic[exercise]} }, Alignment->Left] ];

(*Function that takes an array as input and implements the bubblesort game*)
BubbleSortGame[start_, optimized_:False] := 
	DynamicModule[{originalArray = start, opt = optimized, steps, currentArray, arrayDisplay, trial, swapNumber = 0, enabled = True, i, j},
	steps = Rest[DeleteDuplicates[Part[BubbleSortFrames[originalArray, opt],1]]]; 
	(*list of different steps for the array*)
	currentArray = originalArray;    (*current state of the array*)
	arrayDisplay= CreateArrayGrid[currentArray];       (*grid with the current array*)
	message = "";           (*info message*)

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
								message = Style["Scambio giusto!", Darker[Green]]; 
								swapNumber = swapNumber +1;      (*update the number of swaps*)
								(*display the new array*)
								currentArray = trial;
								arrayDisplay= CreateArrayGrid[currentArray],
								(*if the user was wrong*) 
								message = Style["Scambio sbagliato!", Red]],
								(*the user selected less or more than two elements*)
								message = Style["Numero di elementi da scambiare non valido", Red]; 
								(*reset the selection*)
								selection = List[]]], 
					Enabled -> Dynamic[enabled]];

	(*button that the user presses when they think they're done*)
	finishButton = Button["Ho finito!", 
					Dynamic[If[swapNumber == Length[steps],   (*check if they reached the final step*)
								enabled = False;    (*disable the swap button*)
								message = Style["Complimenti, il vettore \[EGrave] ordinato! \[HappySmiley]", Darker[Green], Bold],
								message = Style["Non hai ancora finito...", Red]]]];

	(*display elements of the game*)
	gameGrid = Dynamic[Grid[{{selectionBar}, {swapButton, finishButton}}, Alignment->Left]];
	Dynamic[Grid[{{gameGrid}, {Dynamic[message]}}, Alignment->Left]]];  

(*Function to plot the Bubble Sort complexity compared to x squared*)	
PlotBubbleSort[lunghezza_]:=
	Manipulate[ 
		(*compute the number of bubblesort swaps for lists with different length (up to the user input)*)
		timingBubbleSort = Table[{n,BubbleSortComparisons[n]}, {n,50,lunghezza, 50}];
		(*plot x squared in blue*)
		Show[Plot[n^2, {n,0,500}, PlotLegends-> {Superscript["n",2]} ,
			PlotStyle -> Blue, AxesLabel->{"lunghezza del vettore", "Numero di confronti"}],
		(*plot the number of swaps for the lists*)
		ListPlot[timingBubbleSort , PlotStyle-> {Red, PointSize[Medium]}, PlotLegends-> {"Bubble Sort"} ], PlotRange->All],
		{lunghezza, 50,500,50}, ControlPlacement->Top
	]

(*Function to plot the Bubble Sort complexity compared to its optimized version*)
PlotOptimizedBubbleSort[lunghezza_]:=
	Manipulate[ 
		(*create a list of 500 random elements*)
		list = Table[RandomInteger[9],500];
		(*compute the number of bubblesort swaps for lists with different length (up to the user input)*)
		timingBubbleSort = Table[{n,BubbleSortComparisons[n]}, {n,50,lunghezza, 50}];
		(*do the same with optimized bubblesort*)
		optTimingBubbleSort = Table[{n,BubbleSortComparisons[n, True]}, {n,50,lunghezza, 50}];
		(*plot the number of swaps of the different bubblesort versions*)
		ListPlot[{timingBubbleSort ,optTimingBubbleSort }, PlotLegends-> {"classico","ottimizzato"}, 
				PlotMarkers -> {Automatic, Medium}, AxesLabel->{"lunghezza del vettore", "Numero di confronti"}],
		{lunghezza, 50,500,50}, ControlPlacement->Top
	]

(*function that takes the input answer and check if it is correct *)
BubbleSortQuiz[start_]:=
	DynamicModule[{array = start, n = Length[start], countpass = 0, input1= Null,input2= Null,input3= Null}, 
	(*calculate the number of exchanges first pass*)
	Do[
		(*compare adjacent elements*)
		If[CardToNumber[array[[j]]]> CardToNumber[array[[j+1]]], 
			(*swap the elements*)
			array = Swap[array, j, j+1]; 
			(*storage variable of exchanges iteration*)
			countpass=countpass+1, True],   
		{j,1,n-1}];  
							
	element1 = InputField[Dynamic[input1],Number, FieldHint->"Inserici valore", FieldSize->Small]; 
	element2 = InputField[Dynamic[input2],Number, FieldHint->"Inserici valore", FieldSize->Small];
	element3 = InputField[Dynamic[input3],Number, FieldHint->"Inserici valore", FieldSize->Small];
	message1 = "";
	message2 = "";
	message3 = "";
	message4 = "";

		 
	(*check if the number of pass is correct*)
	 answerButton1 = Button["Controlla",
						Dynamic[If [input1 == n-1, 
							message1 = Style["Risposta corretta!",Darker[Green], Bold], (*if the answer is correct*)
							message1 = Style["Risposta sbagliata!",Red,Bold]](*if the answer is wrong*) 
							]];

	(*check if the number of comparisons is correct*)
	 answerButton2 = Button["Controlla",		
						Dynamic[If[ input2 == (n-1)^2, 
							message2 = Style["Risposta corretta!",Darker[Green], Bold], (*if the answer is correct*)
							message2 = Style["Risposta sbagliata!",Red,Bold]](*if the answer is wrong*) 
							]];
							
	
	(*check if number of exchanges first iteration is correct*)	
	answerButton3 = Button["Controlla",	
						Dynamic[If[input3 == countpass, 
							message3 = Style["Risposta corretta!",Darker[Green], Bold], (*if the answer is correct*)
							message3 = Style["Risposta sbagliata!",Red,Bold]] (*if the answer is wrong*) 
							]];
			
							
	ClearAll[input1,input2,input3];
							
	(*return the graphics elements*)
	arr=Row[Part[start],Background->Lighter[Gray, 0.5]];
	prima=Row[{Text["Quante passate deve fare l'algoritmo per riordinare il vettore?"], Spacer[19], element1, Spacer[20], answerButton1, Spacer[20], Dynamic[message1]}];
	seconda=Row[{Text["Quanti confronti deve fare l'algoritmo per riordinare il vettore?"], Spacer[20],element2,Spacer[20], answerButton2, Spacer[20], Dynamic[message2]}];
	terza=Row[{Text["Quanti scambi deve fare l'algoritmo nella prima passata?"], Spacer[44], element3, Spacer[20], answerButton3, Spacer[20], Dynamic[message3]}];
	Column[{arr, Text[Style["QUIZ:", Large, Bold, Red]], prima, seconda, terza}]]; 
	

(* Function that prints the start button *)	
PrintStartButton[]:= 
	Module[{startButton, startLink},
	(* creates the start button*)
	startButton =
		Hyperlink[
			Button[
				startIcon, 
				ImageSize->{60,60}, 
				Appearance->{None}
			],
		{EvaluationNotebook[],"FirstChapter"}];
		
	(*creates the start hyperlink*)
	startLink = 
		Hyperlink[
			"Inizia!", 
			{EvaluationNotebook[], "FirstChapter"},
			BaseStyle->RGBColor[0.,0.86,0.37]
		];

	(* returns the graphic elements*)
	Grid[
		{{startButton,startLink}},
		Alignment->{{Right, Left}}, 
		ItemSize->{{Scaled[0.45],Scaled[0.52]}}, 
		Frame->All, 
		FrameStyle->RGBColor[1.,1.,1.], 
		Spacings->{2,6},
		ItemStyle->Directive[FontFamily->Default, FontSize->30, FontWeight->Bold]
	]];

End[]

EndPackage[]


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
