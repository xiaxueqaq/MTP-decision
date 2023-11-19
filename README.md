# MTP-decision
This project provides a decision algorithm to the first-order theory of univariate MTPs. The implementation is based on MATHEMATICA 12. 

You can find our paper at [[ISSAC' 23, Chen and Xia]](https://dl.acm.org/doi/abs/10.1145/3597066.3597104). Also the slides presented at the conference are included in this repository.

## **UPDATE!**
A preprocessor is added so now it can directly handle first-order formulas! Use the newly introduced function `ProveMTP[$\phi$]` to prove MTP propositions. See the usage below.

# Dependency
Mathematica 12

# Install
Download `UnivariateMTPDecisionV2.wl`

# How to load the package
## Method 1
Open `UnivariateMTPDecisionV2.wl`;

Click "Run All Codes" Button;

Now the commands `DecideMTP` and `ProveMTP` are ready for use.
## Method 2
Call `Import[filepath]`, where `filepath` is the path that you storage `UnivariateMTPDecisionV2.wl`;

Now the commands `DecideMTP` and `ProveMTP` are ready for use.

# Usage
The package includes four public functions:
 - `MTPCanonicalForm[f,x]` takes an MTP $f$ in $x$, return the canonical form of $f$, see Definition 2.10;
 - `GetBound[f,x]` takes an MTP $f$ in $x$, return the bound $k_-$, $k_+$, see Corollary 3.4;
 - `DecideMTP[$\phi$,bv,mtps,x,flag:True]` takes a quantifier-free formula $\phi$ whose atoms are of the form $b_i~?~0$, $bv$ is a list of boolean variables ${b_1,\ldots,b_s}$, $mtps$ is a list of MTPs ${f_1,\ldots,f_s}$, $x$ is the variable in MTPs. If $flag$ is the default value true, this function returns the truth value of $\forall x \Phi(x)$, where $\Phi(x)$ is the formula replacing all $b_i$ in $\phi$ with $f_i$; Else $flag$ is false and this function returns the truth value of $\exists x \Phi(x)$;
 - `ProveMTP[$\phi$]` takes a closed FOL in the theory of MTPs, and returns its truth value.

# Examples
For example, suppose we want to prove 
$$(\forall x)\left(\left(\left(x^2\cos x-\sin x=0\right)\wedge \left(x>0\right)\right)\implies\left(x-1>0\right)\right).$$

Call `DecideMTP[((b1 == 0) \[And] (b2 > 0)) \[Implies] (b3 > 0), {b1, b2, b3}, {x^2 Cos[x] - Sin[x], x, x - 1}, x]`, and the function returns `True`.

Another example is from the National College Entrance Examination 2023 (NCEE2023) in China. Problem 22 asks students to prove when $0 < x < 1$, we have $x-x^2 < \sin x < x$. This can be shown by our decision procedure with the call:

`DecideMTP[((b1 > 0) \[And] (b2 < 0)) \[Implies] ((b3 > 0) \[And] (b4 < 0)), {b1, b2, b3, b4}, {x, x - 1, Sin[x] - x + x^2,  Sin[x] - x}, x]`, which returns `True` in 0.125s.

**UPDATE**: You can also use `ProveMTP` now, here shows how to use `ProveMTP` to solve the same problem:

`ProveMTP[ForAll[x, x^2 Cos[x] - Sin[x] == 0 && x > 0 \[Implies] -1 + x > 0]]`, which returns `True`.

`ProveMTP[ForAll[x, ((x > 0) \[And] (x - 1 < 0)) \[Implies] ((Sin[x] - x + x^2 > 0) \[And] (Sin[x] - x < 0))]]`, which returns `True`.

The notebook `UnivariateMTPDecisionV2-Timing.nb` contains all examples in the paper.
