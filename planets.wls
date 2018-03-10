#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input::Initialization:: *)
J2000=DateObject[{2000,1,1,12,0,0},"Instant","Gregorian","Zulu"];
Options[Epochal]={
Epoch->J2000,
Unit->"Seconds",
Convert->UnitConvert
};
Epochal[t_,OptionsPattern[]]:=
With[{
epoch=OptionValue[Epoch],
unit=OptionValue[Unit],
convert=OptionValue[Convert]},
convert[t-epoch,unit]
]

HexifyColor[color_RGBColor]:=StringJoin["0x",IntegerString[Round[Level[color,1]*255],16,2]]

ReducedMass[m1_,m2_]:=FormulaData["ReducedMass",{"m1"->m1,"m2"->m2}]//Last

UnitRule[unit_]:=q_Quantity/;CompatibleUnitQ[q,unit]->UnitConvert[q,unit]
SetAttributes[UnitRule,Listable];
ToUnits[unit_]:=ReplaceAll[UnitRule[unit]]
ToMags[expr_]:=expr/.Quantity[m_,_]->m

ScalaNumberFormat[mantissa_,base_,exponent_]:=
Row[{mantissa,If[exponent=="","","e"],exponent}]

SetOptions[EngineeringForm,NumberFormat->ScalaNumberFormat];

Formatted[expr_] :=
expr//RightComposition@@Thread@Replace@{
color_RGBColor:>HexifyColor[color],
date_DateObject:>DateString[date,"ISODateTime"],
n_?NumberQ:>OutputForm[EngineeringForm[n]],
Quantity[m_,_]:>OutputForm[EngineeringForm[m]]
}

QualifyProperty[qualifier_Rule,p_]:=
p/.{
EntityProperty[type_,name_,qualifiers_List:{}]:>
EntityProperty[type,name,Join[qualifiers,{qualifier}]],
EntityProperty[type_,name_,qualifiers_List,transform_]:>
EntityProperty[type,name,Join[qualifiers,{qualifier}],transform]
}
SetAttributes[QualifyProperty,Listable];
QualifyProperty[qualifier_Rule][p_]:=QualifyProperty[qualifier,p]
DatedP[date_][p_]:=QualifyProperty["Date"->date,p]

ToCSV[data:{_List..}]:=ExportString[Normal[data],"CSV",CharacterEncoding->"UTF-8"]
ToCSV[data:{_List..},file_]:=Export[file,Normal[data],"CSV",CharacterEncoding->"UTF-8"]
ToCSV[data:{_Association..},rest___]:=
ToCSV[
Prepend[
data//Map[Values]//MapAt[Formatted,{All,All}],
data//First//Keys
],
rest
]

Options[Retry]={
Timeout->Infinity,
MaxIterations->Infinity
}
Retry[f_,cond_,OptionsPattern[]]:=
Module[{result,i},
TimeConstrained[
For[
i=0;result=f,
i<OptionValue[MaxIterations]&&cond[result],
i++;Print["retrying"];result=f
];
result,
OptionValue[Timeout]
]
]

SetAttributes[Retry,HoldAll];

RetryEntityValue[args__]:=
Retry[
EntityValue[args],
FirstCase[#,_?MissingQ->True,False,Infinity]&,
Timeout->Quantity[1,"Minutes"],
MaxIterations->4
]


(* ::Text::Initialization:: *)
(**)


(* ::Input::Initialization:: *)
elements=(
Join[
Join[
EntityValue[EntityClass["Planet",All],CanonicalName/@{EntityProperty["Planet","Name"],EntityProperty["Planet","Apoapsis"],EntityProperty["Planet","AverageOrbitDistance"],EntityProperty["Planet","AverageOrbitVelocity"],EntityProperty["Planet","Color"],EntityProperty["Planet","Eccentricity"],EntityProperty["Planet","Glyph"],EntityProperty["Planet","HillRadius"],EntityProperty["Planet","Mass"],EntityProperty["Planet","OrbitalAngularMomentum"],EntityProperty["Planet","OrbitCenter"],EntityProperty["Planet","OrbitPeriod"],EntityProperty["Planet","Periapsis"],EntityProperty["Planet","PeriapsisArgument"],EntityProperty["Planet","PeriapsisLongitude"],EntityProperty["Planet","PeriapsisTimeLast"],EntityProperty["Planet","Radius"],EntityProperty["Planet","RocheLimit"],EntityProperty["Planet","SemimajorAxis"],EntityProperty["Planet","SemiminorAxis"],EntityProperty["Planet","SphereOfInfluenceRadius"]},
"PropertyAssociation"],
EntityValue[{Entity["MinorPlanet","Pluto"]},CanonicalName/@{EntityProperty["MinorPlanet","Name"],EntityProperty["MinorPlanet","Apoapsis"],EntityProperty["MinorPlanet","AverageOrbitDistance"],EntityProperty["MinorPlanet","AverageOrbitVelocity"],EntityProperty["MinorPlanet","Color"],EntityProperty["MinorPlanet","Eccentricity"],EntityProperty["MinorPlanet","Glyph"],EntityProperty["MinorPlanet","HillRadius"],EntityProperty["MinorPlanet","Mass"],EntityProperty["MinorPlanet","OrbitalAngularMomentum"],EntityProperty["MinorPlanet","OrbitCenter"],EntityProperty["MinorPlanet","OrbitPeriod"],EntityProperty["MinorPlanet","Periapsis"],EntityProperty["MinorPlanet","PeriapsisArgument"],EntityProperty["MinorPlanet","PeriapsisLongitude"],EntityProperty["MinorPlanet","PeriapsisTimeLast"],EntityProperty["MinorPlanet","Radius"],EntityProperty["MinorPlanet","RocheLimit"],EntityProperty["MinorPlanet","SemimajorAxis"],EntityProperty["MinorPlanet","SemiminorAxis"],EntityProperty["MinorPlanet","SphereOfInfluenceRadius"]},"PropertyAssociation"]
]//Map[
<|#,
"Glyph"->FromEntity[#Glyph]
|>&
],
EntityValue[EntityClass["PlanetaryMoon",All],CanonicalName/@{EntityProperty["PlanetaryMoon","Name"],EntityProperty["PlanetaryMoon","Apoapsis"],EntityProperty["PlanetaryMoon","AverageOrbitDistance"],EntityProperty["PlanetaryMoon","AverageOrbitVelocity"],EntityProperty["PlanetaryMoon","Color"],EntityProperty["PlanetaryMoon","Eccentricity"],EntityProperty["PlanetaryMoon","HillRadius"],EntityProperty["PlanetaryMoon","Mass"],EntityProperty["PlanetaryMoon","OrbitalAngularMomentum"],EntityProperty["PlanetaryMoon","OrbitCenter"],EntityProperty["PlanetaryMoon","OrbitPeriod"],EntityProperty["PlanetaryMoon","Periapsis"],EntityProperty["PlanetaryMoon","AscendingNodeLongitude"],EntityProperty["PlanetaryMoon","PeriapsisArgument"],EntityProperty["PlanetaryMoon","PeriapsisTimeLast"],EntityProperty["PlanetaryMoon","Radius"],EntityProperty["PlanetaryMoon","RocheLimit"],EntityProperty["PlanetaryMoon","SemimajorAxis"],EntityProperty["PlanetaryMoon","SemiminorAxis"]},
"PropertyAssociation"]//RightComposition[
Select[Not@MissingQ[#Mass]&],
Select[Not@MissingQ[#Apoapsis]&],
Select[#Mass>=Entity["PlanetaryMoon","Deimos"][EntityProperty["PlanetaryMoon","Mass"]]&],
Map[
<|#,
"Glyph"->"",
"Color"->ToEntity[LightGray],
"PeriapsisLongitude"->
(\[CapitalOmega]+\[Omega])/.{
\[CapitalOmega]->#AscendingNodeLongitude,
\[Omega]->#PeriapsisArgument
},
"PeriapsisTimeLast"->J2000,
"SphereOfInfluenceRadius"->
0.9431 a (m/M)^(2/5)/.{
a-> #SemimajorAxis,
m->#Mass,
M->#OrbitCenter["Mass"]
}
|>&
],
Map[KeyDrop["AscendingNodeLongitude"]]
]
]//RightComposition[
Map[KeySort],
Map[
Append[#,
Module[{centralMass,reducedMass,color},
centralMass=#OrbitCenter["Mass"];
reducedMass=ReducedMass[centralMass,#Mass];
color=FromEntity[#Color];
{
"CentralMass"->centralMass,
"ReducedMass"->reducedMass,
"SpecificOrbitalAngularMomentum"->#OrbitalAngularMomentum/reducedMass,
"OrbitCenter"->CanonicalName[#OrbitCenter],
"Color"->color,
"PeriapsisTimeLastJ2000"->Epochal[#PeriapsisTimeLast]
}
]
]&
],
ToUnits[{Quantity[1,"Meters"],Quantity[1,"Kilograms"],Quantity[1,(("Meters")/("Seconds"))],Quantity[1,"Kilograms" ("Meters")^2]/Quantity[1,"Seconds"],Quantity[1,((("Meters")^2)/("Seconds"))] ,Quantity[1,"Seconds"],Quantity[1,"Radians"]}],
MapAt[Evaluate,{All,All}]
]
);

ToCSV[elements,"elements.csv"];
Dataset[elements]


(* ::Text::Initialization:: *)
(**)


(* ::Input::Initialization:: *)
systemState[planets:{Entity["Planet",__]..},time_DateObject,epoch_DateObject]:=
With[{
systemData=
RetryEntityValue[planets,Flatten@{
 {{EntityProperty["Planet","Name"],EntityProperty["Planet","PeriapsisLongitude"],EntityProperty["Planet","Eccentricity"]}},
 {{EntityProperty["Planet","TrueAnomaly"],EntityProperty["Planet","DistanceFromSun"],EntityProperty["Planet","VelocityAroundSun"]}//DatedP[time]}
}]
//ToUnits[{"Radians","Meters","Meters/Second"}]
},
systemData/.{
{name_,\[CurlyPi]_,e_,\[Theta]_,r_,v_}:>
Module[{l,\[Phi],\[Alpha],\[Angle]},
\[Angle]=Quantity[1,QuantityUnit[\[Theta]]];
l=\[Theta]+\[CurlyPi];
\[Phi]=\[Angle] ArcTan[1+e Cos[\[Theta]],e Sin[\[Theta]]];
\[Alpha]=l+\[Angle] \[Pi]/2-\[Phi];
{Subscript[r, x],Subscript[r, y]}=AngleVector[{r,l}];
{Subscript[v, x],Subscript[v, y]}=AngleVector[{v, \[Alpha]}];
<|
"Time"->time,
"TimeEpoch"->Epochal[time,Epoch->epoch],
"Name"->name,
"r"->r,
"l"->l,
"v"->v,
"\[Alpha]"->\[Alpha],
"rx"->Subscript[r, x],
"ry"->Subscript[r, y],
"vx"->Subscript[v, x],
"vy"->Subscript[v, y]
|>
] 
}
]

systemState[planets:{Entity["Planet",__]..},times:{___DateObject},epoch_DateObject:J2000]:=
Table[
systemState[planets,time,epoch],
{time,times}
]//RightComposition[
Flatten,
SortBy[{#TimeEpoch&,#r&}]
]

systemState[
planets:{Entity["Planet",__]..},
{start_DateObject,
interval_?(q\[Function]CompatibleUnitQ[q,Quantity[1,"Seconds"]]),
n_?NumberQ},
epoch_DateObject:J2000
]:=
systemState[planets,Table[start+i interval,{i,0,n-1}],epoch]

systemState[planet:Entity["Planet",__],rest___]:=systemState[{planet},rest]


(* ::Text::Initialization:: *)
(**)


(* ::Input::Initialization:: *)
ephemerides=systemState[PlanetData[],{J2000,Quantity[1,"Days"],366}];
ToCSV[ephemerides,"ephemerides.csv"];
Dataset[ephemerides]


(* ::Text::Initialization:: *)
(**)


(* ::Input::Initialization:: *)
planetApsidesCircaJ2000=With[{
times=EntityValue[PlanetData[],{EntityProperty["Planet", "PeriapsisTimeLast", {"Date" -> DateObject[{2000, 1, 1, 12, 0, 0}, "Instant", "Gregorian", "Zulu"]}],EntityProperty["Planet", "PeriapsisTime", {"Date" -> DateObject[{2000, 1, 1, 12, 0, 0}, "Instant", "Gregorian", "Zulu"]}],EntityProperty["Planet", "ApoapsisTimeLast", {"Date" -> DateObject[{2000, 1, 1, 12, 0, 0}, "Instant", "Gregorian", "Zulu"]}],EntityProperty["Planet", "ApoapsisTime", {"Date" -> DateObject[{2000, 1, 1, 12, 0, 0}, "Instant", "Gregorian", "Zulu"]}]}]},
Thread[{PlanetData[],times}]
]//Echo;


(* ::Input::Initialization:: *)
apsides=Table[
Evaluate[systemState[Sequence@@entry,J2000]]/.{
{periapsisTimeLast_,periapsisTime_,apoapsisTimeLast_,apoapsisTime_}->
Sequence[
<|"Tag"->"PeriapsisTimeLast",periapsisTimeLast|>,
<|"Tag"->"PeriapsisTime",periapsisTime|>,
<|"Tag"->"ApoapsisTimeLast",apoapsisTimeLast|>,
<|"Tag"->"ApoapsisTime",apoapsisTime|>
]
},
{entry,planetApsidesCircaJ2000}
]//EchoFunction[Dataset];
ToCSV[apsides,"apsides.csv"];



