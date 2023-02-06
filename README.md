# MTP-decision
This project provides a decision algorithm to the first-order theory of univariate MTPs. The implementation is based on MATHEMATICA 12.
#Dependedncy
Mathematica 12
#Install
Download `UnivariateMTPDecisionV2.wl`
#How to load the package
##Method 1
Open `UnivariateMTPDecisionV2.wl`;
Click "Run All Codes" Button;
Now the `DecideMTP` command is ready for use.
##Method 2
Call `Import[filepath]`, where `filepath` is the path that you storage `UnivariateMTPDecisionV2.wl`;
Now the `DecideMTP` command is ready for use.
#Usage
The package includes three public functions:
 - `MTPCanonicalForm[f,x]` takes an MTP $f$ in $x$, return the canonical form of $f$, see Definition 2.10;
 - `GetBound[f,x]` takes an MTP $f$ in $x$, return the bound $k_-$, $k_+$, see Corollary 3.4;
 - `DecideMTP[\phi,bv,mtps,x,flag]` takes a quantifier-free formula $\phi$ whose atoms are of the form $b_i~?~0$, $bv$ is a list of boolean variables $\{b_1,\ldots,b_s\}$, $mtps$ is a list of MTPs $\{f_1,\ldots,f_s\}$, $x$ is the variable in MTPs. If $flag$ is true, this function returns the truth value of $\forall x \Phi(x)$, where $\Phi(x)$ is the formula replacing all $b_i$ in $\phi$ with $f_i$; Else $flag$ is false and this function returns the truth value of $\exists x \Phi(x)$.
#Examples
The notebook `UnivariateMTPDecisionV2-Timing.nb` contains all examples in the paper.
