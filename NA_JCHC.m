Off[CreateDirectory::ioerr];
Print[StyleForm["=====================================================","Section",FontSize->14]]
Print[StyleForm["PACKAGE:","Section",FontSize->14],StyleForm[" NEWS ANALYSIS","Section",FontSize->14] ]
Print[StyleForm["BY: Jorge Chávez Carlos, 2018","Section",FontSize->12]]
Print[StyleForm["=====================================================","Section",FontSize->14]]
Print[StyleForm["This analizer detect the frequency of a word in a text, in specict for a lot of ","Section",FontSize->12]]
Print[StyleForm["news of differents international reviews.","Section",FontSize->12]]
Print[StyleForm["Link to download:","Section",FontSize->10]]
Print["https://www.dropbox.com/s/x51wzzjp1fo3fve/IB_JCHC.zip"]
Print[StyleForm["Importing data:","Section",FontSize->12]]
(*reviews = Import["revs.txt","Table"];*)
files = FileNames["art*"];
datas = Import[#] & /@ files;
data = Join[datas[[1,2;;-1]],datas[[2,2;;-1]],datas[[3,2;;-1]]];
ld = Length[data];
lgs=Table[Length[data[[i]]],{i,1,ld}];
Print["Imported data complete!."];
Print["Seleccting data with content..."];
ind={};
Do[If[lgs[[i]]==10,AppendTo[ind,i]],{i,1,ld}];
Print[StyleForm["Command list:","Section",FontSize->12]]
Print["Using a NEW data set:"];
Print["WF1['word1'], Returns the data time serie in a file .csv for the word 'word1', finded in the articles of news"];
Print["Ready to identify words!"];
Print[StyleForm["=====================================================","Section",FontSize->14]]
Speak["El paquete fué cargado exitosamente"];

WF1[word1_]:={dateff=ParallelTable[{DateObject[data[[ind[[i]],6]]][[1]],WordFrequency[data[[ind[[i]],10]],word1]},{i,1,65000}];
word=word1;
b=Sort[dateff[[1;;Length[dateff]]]];
dates=With[{first=b[[1,1]]},DateRange[first,b[[-1,1]]]];
inds[i_]:=Position[b[[1;;-1,1]],dates[[i]]];
con[n_]:=Total[Table[Boole[b[[inds[n][[i,1]],2]]!=0],{i,1,Length[inds[n]]}]];dat=Table[{dates[[i]],con[i]},{i,1,Length[dates]}];
Export[ToString[word]<>".csv",dat];
d1=DateListPlot[dat,PlotRange->All,PlotStyle->Black,DateTicksFormat->{"Day","/","MonthShort","/","YearShort"},PlotTheme->"Business",PlotLegends->{word,Black},ImageSize->500];Print["Frequency of the word in the texts"];
Show[d1]};
