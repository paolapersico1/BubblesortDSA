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

(*Function to swap the elements of an array given two indices*)
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
								message = Style["Complimenti, l'array \[EGrave] ordinato! \[HappySmiley]", Darker[Green], Bold],
								message = Style["Non hai ancora finito...", Red]]]];

	(*display elements of the game*)
	gameGrid = Dynamic[Grid[{{selectionBar}, {swapButton, finishButton}}, Alignment->Left]];
	Dynamic[Grid[{{gameGrid}, {Dynamic[message]}}, Alignment->Left]]];  


 (*function that computes the number of exchanges at first pass*)
ComputePass[currentArray_,optimized_]:=
	Module[{array = currentArray, opt = optimized, i, j, len = Length[list], n, frames, final, count},
	(*first element: list of cards, second element: compared indices, third element: final elements*)
	frames = List[List[list],List[{}], List[{}]];
	n = len;
	(*for len-1 times*)
	Do[
		(*update frames*)
			If[opt, final = Last[Part[frames,3]], final ={}]; 
			frames = AppendFrame[frames, array, List[j, j+1], final];
			(*compare adjacent elements and swap if  in wrong order*)
			If[CardToNumber[l[[j]]]> CardToNumber[l[[j+1]]], 
				array = Swap[l, j, j+1];
				count=count+1;
				(*update frames*)
				frames = AppendFrame[frames, array, Last[Part[frames,2]], final],True],
		{j,1,n-1}]; ]

(*Function to start the bubblesort quiz*)
StartBubbleSortQuiz[optimized_:False] := 
	DynamicModule[{opt = optimized, inputArray = Null, len = 6, start,quiz=""},
	(*radiobuttonbar to select the length of the array*)
	lengthBar = RadioButtonBar[Dynamic[len],Range[1,6]];
	(*array in input*)
	elements = InputField[Dynamic[inputArray], FieldHint->"{0,1,2,3,4,5}"]; 

	(*button to create the cards array and start the game*)    
	createButton = Button["Crea array e inizia il quiz", 
	If[SameQ[inputArray, Null], (*if the user hasn't specified an array*)
		(*a random array with the selected length is created*)
		start = Table[NumberToCard[RandomInteger[9]], len  ]; quiz=BubbleSortQuiz[start],
		(*otherwise we check if the input is a list of integers between 0 and 9*)
		If[MatchQ[inputArray, { (0|1|2|3|4|5|6|7|8|9)..}], 
			start = Map[NumberToCard,inputArray ];
			(*if the length of the array is the same as the one selected, start the game*)
			If[Length[start] == len, quiz=BubbleSortQuiz[start,opt],  quiz = "Lunghezza array sbagliata"], 
			quiz = "L'array pu\[OGrave] contenere solo numeri da 0 a 9"]]
			];
			

	(*return the graphics elements*)
	Grid[{{"lunghezza dell'array", lengthBar}, {"array", elements},{createButton}, {}, {Dynamic[quiz]}}]];  
	      	      
	      	      	      	      
(*function that takes the input answer and check if it is correct *)
BubbleSortQuiz[start_, optimized_:False]:=
	DynamicModule[{originalArray = start, opt = optimized, steps,comparisons, currentArray, arrayDisplay,count,input1= Null,input2= Null,input3= Null},
	currentArray = originalArray;    (*current state of the array*)
	arrayDisplay= CreateArrayGrid[currentArray];
	element1 = InputField[Dynamic[input1],Number, FieldHint->"Inserici valore"]; 
	element2 = InputField[Dynamic[input2],Number, FieldHint->"Inserici valore"];
	element3 = InputField[Dynamic[input3],Number, FieldHint->"Inserici valore"];
	message1 = "";
	message2 = "";
	message3 = "";
	 numpassate= steps;            (*number of pass*)
	 numconfronti=comparisons;     (*number comparisons*)
	 numprimapass= count;          (*number of exchanges first pass*)
	 
	 ComputePass[currentArray,opt];
	 
	(*check if the number of pass is correct*)
	 answerButton1 = Button["Controlla",
						Dynamic[If[NumberQ[ input1 == numpassate],
							message1 = "Risposta corretta!", (*if the answare is correct*)
							message1 = "Risposta sbagliata!"] (*if the answare is wrong*) 
							]];

	(*check if the number of comparisons is correct*)
	 answerButton2 = Button["Controlla",		
						Dynamic[If[NumberQ[ input2 == numconfronti], 
							message2 = "Risposta corretta!", (*if the answare is correct*)
							message2 = "Risposta sbagliata!"] (*if the answare is wrong*) 
							]];
							
	
	(*check if number of exchanges first pass is correct*)	
	answerButton3 = Button["Controlla",	
						Dynamic[If[NumberQ[ input3 == numprimpass], 
							message3 = "Risposta corretta!", (*if the answare is correct*)	
							message3 = "Risposta sbagliata!"] (*if the answare is wrong*) 
							]];
							
	ClearAll[input1,input2,input3];
							
	(*return the graphics elements*)						
	Grid[{{arrayDisplay},{}}]
	Grid[{{Text[Style["QUIZ:",Large,Bold,Red]]},{Text["Quante passate fa l'algoritmo per riordinare l'array?"]},{element1, answerButton1, Dynamic[message1]}, 
	{},{Text["Quanti confronti devi fa l'algoritmo per riordinare l'array?"]},
	{element2, answerButton2, Dynamic[message2]}, {},{Text["Quanti scambi devi fare nella prima passata dell'algoritmo?"]},
	{element3, answerButton3, Dynamic[message3]} ,{}}]];  

(* Function that prints the start button *)	
PrintStartButton[]:=

	(* creates the start button*)
	StartButton :=
		Hyperlink[
			Button[
				Image[CompressedData["
1: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
7E1v9LTOhC7jLnL+9Sf6lcpNTsupkuB9n1p/32BsTOHb5GP/O2/OMv0Abptb
wbqknmxHkfd+wRtU8dktsjS03HT9I+Ztyj4++i6M8/Whb/Eu7WOtCVz0b6jb
FnMIKvdxcJtB92+FNg9sKXKBU98VtpOQ3g6L2K67vIx3zDcp7/2CJ4r5XDav
zEyy2meO1k4oYp1IbMS8JX501gu8rDdhywi1Tzzm4Gqk5l+O9b4MY2gp9Q4J
rIlQzCmneyLGRHDbirE/nmo9y3u/4BV8Y1M0eVknZSvGiXPbNUn+XYM+ngHX
pQ+41r1LH4U6ybiLWr/Qrj91qnemjwvt7doVmGMa82Q6bM9N+B2M00Z8w/Ew
1uW9X/BOlJqYeO0b5LZpkiKPwZmO5mty0Ar9KJf6w28s9bDuBCv4TYq6TRtZ
vL9Dnde4OK+EDuY8KfIVuWrXW7newwcS/Qrl+pu/vPcLDKgk3kU1c7nQjOMk
PieUmh44nzCnist5C+f17zu1wdf3xhAl0SdSv4MWvp93O9J56UAy/5mU+lYN
2Mc+ptzlDFjMYlMa/9nhOJf3foEFfJ9spObfVGMZ8+ay5C6bhrlj+imUts0g
WzEXuwsbejz27yTquJEiT+sljvQ9L9R4Nd/AfrmrIo4hbpH9fNqCc9vhGJf3
foENlZkvEo7ljaHkOcJ8udPl8HckG/A92Y0dvRn7h28qLmJGicf7ffom5o9V
q+npmkamX+SopsCdmHPXlx3weyc5Gefy3i8wgu+UirC2Tyi1jbDWmfKXv/xi
zB3sypYejP3bAOc25aItcx81+loEE1hPL5TvXaGR1xdKzXLycQJ7pw/98S1T
0deAlPd+gR2VxqfSjen4dG57ELz3e8rHvR72qc+6Xvd7LPbvqmhIv8xJQ2Jt
7mbNW2Kd9S9lHZ+ZIsfCMHXbY85G57onpp9Y75WhvJEKvQvcWd5DOKZX47d2
bpuK+x35OjONXOtsn5oCRewfrGHf6Uv0znmfN2vbHRsl5i/wz5uVvzxm7ddx
OK/hnd9ljkgX9SxAzggtr2WoFHlzqPMG3+q6HiftHcmcKe/9Ajd5Hovm2k8z
rgOo6efpzX8cv0v7/M6rCGL/YK//VLu/j/6fk2cD+N8drPyeDW7ENxSsu+yy
DbG/VF5jj1T3SzCu1KXedUNl+rWKOs9yEn/Flb74tqfo4kROwhwkrnQVhE7B
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
VDMXhCD4Bu6Vb1HE99tGYj7ObZcr0JcR64bk5wHMjZyYG8DmjZZtlmD8OtwB
F2CtJXx74LaTA9gDPkI1BvtHB3fitqcMGLtov++Z27nt6BYY/38nWnv+yG2L
IFSBvjWLHglz5hbKvR9kMbddvkHfe/ThytfwLD5Q4Vt2En8F/rkI3+3hPnKc
ynMQawP/+UFw1/tklJn3YOwB9gG3/qFQ5GomeQPA+zS3PWUo8iFY273/2BEP
57alLJiXT9HcQcbr/I1MECgp4t8p9/5kYGjhY7ntEqoLno1I7oFwn+S2pSx4
f7e1uz/VL+C2oyyg934kZz70ORIEYVZUov+PeO8fr2sdUsEfhR8ARb3pLVU7
iyqCvHdRot/JbUdZVBqfQrQG7cFtiyCETvHeRrHG3ieZPo7bLqEe5N9MSO6D
+v3ctpSBolZ1lJkDuO0oQ5FzjCIX98pe8JMVBFsaifk16d6fmhujUf0wbruE
eoBv2BTjkrPeZDeAzscS7P+f4bajDGQ5fzI9yG2LIIQO1uFRtHW1tzSG4zdw
2yXUB/ThUjR1AS7jtqUMKjEL7fdB80VuO8pQ1OKyXocG0viF3LYIQuio3A+d
bO/HN9bDuW0S6kOefzE1txGNz81Vyo9cxIzYzcfUxNx2lIHo2/+N3HYIQhWA
+8EVZPt/Yq6sUx5agRfivb+5H8Lf5LarU5o5pqz3/29y21EG0HmFrc2NxHyH
2w5BqAIUMUYtsgLj3rltmsq85Uc/EGt+Y6w96HgMyEkgizGvf55TL40/2Tcy
+GxuPanJ4+eb33cwb/HRTZvjs6PM/D5K9U/B7s9jjWDM98yt61Rc7P3NM2q8
N7dtnQK6NghsPpbbjk5BnyFF8C0Sc2Zx2yIIVQDmyxriNTaYM4Aa0bsXe/09
Hep+PZwTvlb1fHtYYxH3e7j7ZR3aDe2jf4O5H7l1RxrD8cud7P2pfJ8KGVg3
Xk3Qx1urFucpCFzAfFnqYJ1lPQNgjnzQ4VIL/dfDfngk1iDhsqEbmnUJ8rox
3edOS8xF6IPNZYPLvT/f/1P9Wy7bhJmB/tnHuo8TcwO3HYJQFRSx/1+L3OL7
DIDvh1FmfqiI4hmwzh7YsJtPG7qlyJN3J1HfbYHzz7d8+8oVe/9qV3t/Lpm5
0KdNQueQ+Dtm5vfcdtSdohbqJVj3hFsXwQ4H8X/bnAHmZoM7+rCjuPPf5MAG
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
8e0XiO+R3DYLs1PUDCpzNrwtSvUHXesFd/evE4zDq8kVw1w8qVljqxvGD5Lr
NgX4nX1A7ijTXt18k1Cy/5cT2f+DpM77PyJ+gcJ0qJWHPxr66yTVjOts15/4
3y1G3zcvOjXfsW3H4Bbq+JTiLc16fmD+IEq92lG88fz/9u4+xo6qjON4hMib
KBKjohIJSIKgBnwBTYyihEQx/IExG0FjRAO7c2a71CZoURNzZraUNhXEBmIk
8Y0ETGo0+ILBpECMhlSpmmgL0Yq1djszu5WCtJSmpbuew95FKi29s/Ocec6d
+/0kv5Twx+Y+Z+7e372z58787CiPxx/b7y92rQz9Xy/0f5S63v8L2BeIw/H3
oTX+etJV9icz/xl3V++/b3vhPWrb4F8zRHq2tB+SfFz+ukICj+vg6K5Vp0g+
rqM+7h35+5Mqu8PM3693Vy9/9NdJGJvOz2/ysw39T//T/wPT/x77AhE7E8e9
C0Jks/baSjL0f73Q/1Eapv732BeImJnS/jCCrhZPUtrvaa+tJEP/1wv9H6Vh
63+PfYGIlcTvY4xJyzzVXltJhv6vF/o/SsPY/x77AhEjfz1B7a4Okbb3UoRm
6P96of+jNKz9v4B9gYiJe904qaXnY5vZN7Fl7fHaayvJ0P/1Qv9Hadj732Nf
IGLS4j3u2soG7TWVZuj/eqH/o0T/z2NfIGLR+86admdLZq32mkoz9H+90P9R
ov//h32BiEFaZNdG0NliSYrsM9prKs3Q//VC/0eJ/j8U+wKhzV+bRruzJeNe
C9+qvabS3Gveo41fN2fs2dpz9MM/H9170q82yVhpP6Y9R7+M36vV9Hm/7aZT
tefoB/1/eL19gftbeH1kXyAOMTK37lj3vNij3dtCebKLf+tyc21oujZNr0GI
MNyxeabhsT3of4e15+gH/X9k7AuElhb/DhU6v9ReyxD8XI3XZtpeqj0HDuXv
ByHwnN+lPUe/6P+XlhaT57oZt7TwOjnbte9IY/HSKlsaQXc3T5GPaq9lCKaw
dzV/3ezWNZG6wD1f3y3wvH9Me45+0f9H18q+wMp+Q3tOxKP3OeTf6v3dLKW/
noH2WoYgs0fI/kB7DhzKvycTeN7/XnuOftH//Qm6L7DKf+V/vvaMiEtaZVdF
0OGLzWxSZB/XXsNQRM7PVNlUF/dGDDL3nL2n6XF1P+Nu7Tn6Rf/XI70v0O8j
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
4MX+C+GUrEs=
"], "Byte", ColorSpace -> "RGB", Interleaving -> True], 
				ImageSize->{60,60}, 
				Appearance->{None}
			],
		{EvaluationNotebook[],"FirstChapter"}];
		
	(*creates the start hyperlink*)
	StartLink := 
		Hyperlink[
			"Inizia!", 
			{EvaluationNotebook[], "FirstChapter"},
			BaseStyle->RGBColor[0.,0.86,0.37]
		];

	(* returns the graphic elements*)
	Grid[
		{{StartButton,StartLink}},
		Alignment->{{Right, Left}}, 
		ItemSize->{{Scaled[0.45],Scaled[0.52]}}, 
		Frame->All, 
		FrameStyle->RGBColor[1.,1.,1.], 
		Spacings->{2,6},
		ItemStyle->Directive[FontFamily->default, FontSize->30, FontWeight->Bold]
	];

End[]

EndPackage[]


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
