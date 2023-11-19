(* ::Package:: *)

BeginPackage["UnivariateMTPDecisionV2`"];


MTPCanonicalForm::usage= "MTPCanonicalForm takes an MTP f(x, sin x, cos x) and returns its canonical form g(x/2, tan x/2).";


MTPSquareFreePart::usage="MTPSquareFreePart returns the square-free part of an MTP";


GetBound::usage= "GetBound takes an MTP f(x, sin x, cos x) and return k_-, k_+";


DecideMTP::usage = "DecideMTP[\[CapitalPhi](b1,\[Ellipsis],bs),{b1,\[Ellipsis],bs},x,{f1,\[Ellipsis],fs},flag:True] decides the truth value of \[ForAll]x\[CapitalPhi] when flag=True, or the truth value of \[Exists]x\[CapitalPhi] when flag=False";


ProveMTP::usage= "ProveMTP[\[CapitalPhi]] takes a closed formula in FOL of MTPs, and decide the truth of it";


Begin["`Private`"];


MTPCanonicalForm[f_,x_]:=Module[{ret},ret:=f/.{x-> 2x};ret=ret/.{Sin[2x]->(2Tan[x])/(1+Tan[x]^2),Cos[2x]-> (1-Tan[x]^2)/(1+Tan[x]^2)};Return[Together[ret]]];


GetBound[f0_,x_]:=Module[{f,flis,kn,kp,i,t,branchpoint,frominf,S,T,pn,pp,R1,R2},
			     f=Times@@((#[[1]]&)/@FactorSquareFreeList[Numerator[MTPCanonicalForm[f0,x]]/.{Tan[x]-> t}]);
			     flis=((#[[1]]&)/@ FactorList[f]);
                 {kn,kp}={0,0};
                 {pn,pp}={0,0};
			     branchpoint=Solve[Discriminant[f,t]==0,x,Reals];
			     If[Length[branchpoint]>0,
					branchpoint=x/.branchpoint;
					kn=Min[kn,Floor[(Min/@branchpoint)/Pi+1/2]];
					kp=Max[kp,Ceiling[(Max/@branchpoint)/Pi-1/2]];
					pn=Min[pn,Min/@branchpoint];
					pp=Max[pp,Max/@branchpoint];
			     ];
			     frominf=Solve[Coefficient[f,t,Exponent[f,t]]==0,x,Reals];
			     If[Length[frominf]>0,
					frominf=x/.frominf;
					kn=Min[kn,Floor[(Min/@frominf)/Pi+1/2]];
					kp=Max[kp,Ceiling[(Max/@frominf)/Pi-1/2]];
					pn=Min[pn,Min/@frominf];
					pp=Max[pp,Max/@frominf];
				 ];
				 pn=pn-1;
				 pp=pp+1;
                  For[i=1,i<= Length[flis],i++,
                     If[Exponent[flis[[i]],x]>0 || Exponent[flis[[i]],t]>0,
                        (*{kn,kp}=({Min[#1[[1]],#2[[1]]], Max[#1[[2]],#2[[2]]]}&)[{kn,kp},CMBound[flis[[i]],x,t]]*)
                        R1=Resultant[flis[[i]],D[flis[[i]],x]+15/16*(t^2+1)*D[flis[[i]],t],t];
                        R2=Resultant[flis[[i]],D[flis[[i]],x]-15/16*(t^2+1)*D[flis[[i]],t],t];
                        S=Solve[R1==0,x,Reals];
                        T=Solve[R2==0,x,Reals];
                        If[Length[Solve[(flis[[i]]/.{x-> pp})==0,x,Reals]]>0,
                           kp=Max[kp,Ceiling[(Max/@S)/Pi-1/2],Ceiling[(Max/@T)/Pi-1/2]];                           
                        ];
                        If[Length[Solve[(flis[[i]]/.{x-> pn})==0,x,Reals]]>0,
                           kn=Min[kn,Floor[(Min/@S)/Pi+1/2],Floor[(Min/@T)/Pi+1/2]]
                        ];
                     ]
                  ];
			      Return[{kn,kp}];
];


HasRootQ[f0_,x_,l_,r_]:=Module[{t,flis,i},
	If[(l<=0)&&(0<=r)&&((f0/.{x-> 0})== 0),Return[True]];
	If[(l==r),Return[(f0/.{x-> l})==0]];
	flis=((#[[1]]&)/@ FactorList[Numerator[ (MTPCanonicalForm[f0,x]/.{Tan[x]-> t})]]);
	For[i=1,i<= Length[flis],i++,
		If[(flis[[i]]/.{x-> l/2,t-> Tan[l/2]})*(flis[[i]]/.{x-> r/2,t-> Tan[r/2]})<= 0,Return[True]];
	];
	Return[False];
];


MTPSquareFreePart[f_,x_]:=Module[{t},Times@@((#[[1]]&)/@FactorList[Numerator[MTPCanonicalForm[f,x]/.{Tan[x]-> t}]])/.{t-> Tan[x]}];


NewMTPSFP[f_,x_]:=Module[{t,y,z,cf},If[SquareFreeQ[Numerator[MTPCanonicalForm[f,x]/.{Tan[x]-> t}]],Return[f]]; cf=f/.{Sin[x]-> (2 t)/(1+t^2),Cos[x]-> (1-t^2)/(1+t^2)};cf=Together[cf];cf=Times@@((#[[1]]&)/@FactorList[Numerator[cf]]);cf=cf/.{t-> y/(1+z)}; cf=Numerator[Together[cf]]; Return[cf/.{y-> Sin[x],z-> Cos[x]}]]


MTPRootIsolationOnBoundedInterval[f0_,x_,kn_,kp_]:=Module[{R,i,s0,ret,flag},
	(*R=Solve[(f0== 0)&&((2*kn-1)*Pi< x<(2*kp+1)*Pi),x,Reals];*)
	(*To avoid Sort[] cannot sort multple roots *)
	R=Solve[(NewMTPSFP[f0,x]==0)&&((2*kn-1)*Pi<x<(2*kp+1)*Pi),x,Reals];
	If[(f0/.{x-> Pi})==0,R=Join[R,Table[{x-> (2*k-1)Pi},{k,kn+1,kp}]]];
	If[Length[R]==0,Return[{}],R=Sort[DeleteDuplicates[x/. R],#1<#2 &]];
	ret={};
	s0=1024;
	For[i=1,i<= Length[R],i++,
		If[OddQ[R[[i]]/Pi],AppendTo[ret,{R[[i]],R[[i]]}],AppendTo[ret,{Floor[R[[i]]*s0]/s0,Ceiling[R[[i]]*s0]/s0}]];
	];
	While[True,
		flag=True;
		For[i=1,i<Length[R],i++,
			If[ret[[i+1,1]]-ret[[i,2]]<= 0,flag=False;Break[]];
		];
		If[flag,Break[]];
		s0=s0*16;
		For[i=1,i<= Length[R],i++,
			If[Not[OddQ[R[[i]]/Pi]],ret[[i]]={Floor[R[[i]]*s0]/s0,Ceiling[R[[i]]*s0]/s0}];
		]
	];
	Return[ret];
];


DecideMTP[phi_,bv_,mtps_,x_,flag_:True]:=Module[{kn,kp,intv,tmp,i,j,l,r,sap,s0},
	{kn,kp}=GetBound[Times@@mtps,x];
	Print[kn-1,",",kp+1];
	intv=MTPRootIsolationOnBoundedInterval[Times@@mtps,x,kn-1,kp+1];
	Print[intv];
	If[Length[intv]==0,
		tmp={};
		For[j=1,j<= Length[mtps],j++,
			AppendTo[tmp,(bv[[j]]-> Sign[mtps[[j]]/.{x-> 0}])]
		];
		If[flag,
			If[phi/.tmp,Return[True],Return[{False,{0}}]],
			If[phi/.tmp,Return[{True,{0}}],Return[False]]
		]
	];
	For[i=1,i<= Length[intv],i++,
		tmp={};
		For[j=1,j<= Length[mtps],j++,
			If[HasRootQ[mtps[[j]],x,intv[[i,1]],intv[[i,2]]],
				AppendTo[tmp,(bv[[j]]-> 0)],
				AppendTo[tmp,(bv[[j]]-> Sign[mtps[[j]]/.{x-> (intv[[i,1]]+intv[[i,2]])/2}])]
			]
		];
		Print["\[Xi]_",i,":",bv/.tmp];
		If[flag,
			If[Not[phi/.tmp],Return[{False,intv[[i]]}]],
			If[phi/.tmp,Return[{True,intv[[i]]}]]
		]
	];	
	r=Join[((#[[1]]&)/@intv),{(2*kp+3)*Pi}];
	l=Join[{(2*kn-3)*Pi},((#[[2]]&)/@intv)];
	(*Print[l,r];*)
	For[i=1,i<= Length[intv]+1,i++,
		If[(\[Not]OddQ[l[[i]]/Pi])\[And](\[Not]OddQ[r[[i]]/Pi]),sap=(l[[i]]+r[[i]])/2,
			If[OddQ[l[[i]]/Pi],
				s0=1024;
				sap=Ceiling[l[[i]]*s0]/s0;
				While[sap>= r[[i]],s0=s0*8;sap=Ceiling[l[[i]]*s0]/s0]
				,
				s0=1024;
				sap=Floor[r[[i]]*s0]/s0;
				While[sap<= l[[i]],s0=s0*8;sap=Floor[r[[i]]*s0]/s0]
			]
		];
		tmp={};
		For[j=1,j<=Length[mtps],j++,
			AppendTo[tmp,(bv[[j]]-> Sign[mtps[[j]]/.{x-> sap}])]
		];
		Print["(\[Xi]_",i-1,",\[Xi]_",i,"):",bv/.tmp];
		If[flag,
			If[Not[phi/.tmp],Return[{False,sap}]],
			If[phi/.tmp,Return[{True,sap}]]
		]
	];
	If[flag,Return[True],Return[False]];
];


DecideMTPNew[phi_,bv_,mtps_,x_,flag_:True]:=Module[{kn,kp,intv,tmp,i,j,l,r,sap,s0},
	{kn,kp}=GetBound[Times@@mtps,x];
	PrintTemporary[kn-1,",",kp+1];
	intv=MTPRootIsolationOnBoundedInterval[Times@@mtps,x,kn-1,kp+1];
	PrintTemporary[intv];
	If[Length[intv]==0,
		tmp={};
		For[j=1,j<= Length[mtps],j++,
			AppendTo[tmp,(bv[[j]]-> Sign[mtps[[j]]/.{x-> 0}])]
		];
		If[flag,
			If[phi/.tmp,Return[True],Return[{False,{0}}]],
			If[phi/.tmp,Return[{True,{0}}],Return[False]]
		]
	];
	For[i=1,i<= Length[intv],i++,
		tmp={};
		For[j=1,j<= Length[mtps],j++,
			If[HasRootQ[mtps[[j]],x,intv[[i,1]],intv[[i,2]]],
				AppendTo[tmp,(bv[[j]]-> 0)],
				AppendTo[tmp,(bv[[j]]-> Sign[mtps[[j]]/.{x-> (intv[[i,1]]+intv[[i,2]])/2}])]
			]
		];
		PrintTemporary["\[Xi]_",i,":",bv/.tmp];
		If[flag,
			If[Not[phi/.tmp],Return[{False,intv[[i]]}]],
			If[phi/.tmp,Return[{True,intv[[i]]}]]
		]
	];	
	r=Join[((#[[1]]&)/@intv),{(2*kp+3)*Pi}];
	l=Join[{(2*kn-3)*Pi},((#[[2]]&)/@intv)];
	(*Print[l,r];*)
	For[i=1,i<= Length[intv]+1,i++,
		If[(\[Not]OddQ[l[[i]]/Pi])\[And](\[Not]OddQ[r[[i]]/Pi]),sap=(l[[i]]+r[[i]])/2,
			If[OddQ[l[[i]]/Pi],
				s0=1024;
				sap=Ceiling[l[[i]]*s0]/s0;
				While[sap>= r[[i]],s0=s0*8;sap=Ceiling[l[[i]]*s0]/s0]
				,
				s0=1024;
				sap=Floor[r[[i]]*s0]/s0;
				While[sap<= l[[i]],s0=s0*8;sap=Floor[r[[i]]*s0]/s0]
			]
		];
		tmp={};
		For[j=1,j<=Length[mtps],j++,
			AppendTo[tmp,(bv[[j]]-> Sign[mtps[[j]]/.{x-> sap}])]
		];
		PrintTemporary["(\[Xi]_",i-1,",\[Xi]_",i,"):",bv/.tmp];
		If[flag,
			If[Not[phi/.tmp],Return[{False,sap}]],
			If[phi/.tmp,Return[{True,sap}]]
		]
	];
	If[flag,Return[True],Return[False]];
];


ExtractMTP[exp_]:=Module[{},
	If[(exp[[0]]==Implies)||(exp[[0]]==And)||(exp[[0]]==Or||exp[[0]]==Xor||exp[[0]]==Equivalent),Return[Join@@Table[ExtractMTP[exp[[i]]],{i,Length[exp]}]]];
	If[(exp[[0]]==Not),Return[ExtractMTP[exp[[1]]]]];
	Return[{exp}];
];


MTPExpressionPreprocess[exp0_]:=Module[{exp,repOps,ret,mtps,bv,var,flag},
	exp=exp0[[2]];
	repOps=Alternatives@@{LessEqual,GreaterEqual,Less,Greater,Equal,Unequal};
	ret=exp/.op_[a_,b_]/;MatchQ[op,repOps]:>op[a-b,0];
	mtps=DeleteDuplicates[ExtractMTP[(ret)/.op_[a_,0]/;MatchQ[op,repOps]:>a]];
	bv=Array[b,Length[mtps]];
	ret=ret/.Table[mtps[[i]]-> bv[[i]],{i,Length[mtps]}];
	var=exp0[[1]];
	If[exp0[[0]]==Exists,flag=False,flag=True,flag=True];
	(*Print[ret,",",bv,",",mtps,",",var,",",flag];*)
	Return[{ret,bv,mtps,var,flag}];
]


ProveMTP[formula_]:=DecideMTPNew@@(MTPExpressionPreprocess[formula]);


End[];


EndPackage[];
