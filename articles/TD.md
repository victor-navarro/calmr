# TD

## The mathematics behind TD

The temporal difference (TD) model (Sutton & Barto, 1990) is an
extension of the ideas underlying the RW model (Rescorla & Wagner,
1972). Most notably the TD model abandons the construct of a “trial”,
favoring instead time-based formulations. Also notable is the
introduction of eligibility traces, which allow the model to bridge
temporal gaps and deal with the credit assignment problem.

*Implementation note: As of `calmr` version `0.6.2`, stimulus
representation in TD is based on complete serial compounds (i.e.,
time-specific stimulus elements entirely discriminable from each other),
and the eligibility traces are of the replacing type.*

*General Note: There are several descriptions of the TD model out there,
however, all of the ones I found were opaque when it comes to
implementation. Hence, the following description of the model has a
focus on implementation details.*

## 1 - Maintaining stimulus representations

TD maintains stimulus traces as eligibility traces. The eligibility of
stimulus $i$ at time $t$, $e_{i}^{t}$, is given by:

$$e_{i}^{t} = e_{i}^{t - 1}\sigma\gamma + x_{i}^{t}$$

where $\sigma$ and $\gamma$ are decay and discount parameters,
respectively, and $x_{i}^{t}$ is the activation of stimulus $i$ at time
$t$ (1 or 0 for present and absent stimuli, respectively).

Internally, $e_{i}$ is represented as a vector of length $d$, where $d$
is the number of stimulus compounds (not in the general sense of the
word compound, but in terms of complete serial compounds, or CSC). For
example, a 2s stimulus in a model with a time resolution of 0.5s will
have a $d = 4$, and the second entry in that vector represents the
eligibility of the compound active after the stimulus has been present
for 1s.

Similarly, $x_{i}^{t}$ entails the specific compound of stimulus $i$ at
time $t$, and not the general activation of $i$ at that time. For
example, suppose two, 2s stimuli, $A$ and $B$ are presented with an
overlap of 1s, with $A$’s onset occurring first. Can you guess what
stimulus compounds will be active at $t = 2$ with a time resolution of
0.5s?[¹](#fn1)

## 2 - Generating expectations

The TD model generates stimulus expectations[²](#fn2) based on the
presented stimuli, **not on the strength of eligibility traces**. The
expectation of of stimulus $j$ at time $t$, $V_{j}^{t}$, is given by:

$$V_{j}^{t} = w_{j}^{t\prime}x^{t} = \sum\limits_{i}^{K}w_{i,j}^{t}x_{i}^{t}$$

Where $w_{j}^{t}$ is a matrix of stimulus weights at time $t$ pointing
towards $j$, $\prime$ denotes transposition, and $w_{i,j}$ denotes an
entry in a square matrix denoting the association from $i$ to $j$. As
with the eligibility traces above, the entries in each matrix are the
weights of specific stimulus compounds.

Internally, the $w_{j}^{t}$ is constructed on a trial-by-trial,
step-by-step basis, depending on the stimulus compounds active at the
time.

## 3 - Learning associations

Owing to its name, the TD model updates associations based on a
temporally discounted prediction of upcoming stimuli. This temporal
difference error term is given by:

$$\delta_{j}^{t} = \lambda_{j}^{t} + \gamma V_{j}^{t} - V_{j}^{t - 1}$$

where $\lambda_{j}$ is the value of stimulus $j$ at time $t$, which also
determines the asymptote for stimulus weights towards $j$.

The temporal difference error term is used to update $w$ via:

$$w_{i,j}^{t} = w_{i,j}^{t} + \alpha_{i}\beta\left( x_{j}^{t} \right)\delta_{j}^{t}e_{i}^{t}$$

where $\alpha_{i}$ is a learning rate parameter for stimulus $i$, and
$\beta\left( x_{j} \right)$ is a function that returns one of two
learning rate parameters ($\beta_{on}$ or $\beta_{off}$) depending on
whether $j$ is being presented or not at time $t$.

## 4 - Generating responses

As with many associative learning models, the transformation between
stimulus expectations and responding is unspecified/left in the hands of
the user. The TD model does not return a response vector, but it
suffices to assume that responding is the identity function on the
expected stimulus values, as follows:

$$r_{j}^{t} = V_{j}^{t}$$

### References

Rescorla, R. A., & Wagner, A. R. (1972). A theory of Pavlovian
conditioning: Variations in the effectiveness of reinforcement and
nonreinforcement. In A. H. Black & W. F. Prokasy (Eds.), *Classical
conditioning II: Current research and theory.* (pp. 64–69).
Appleton-Century-Crofts.

Sutton, R. S., & Barto, A. G. (1990). Time-derivative models of
Pavlovian reinforcement. In M. Gabriel & J. W. Moore (Eds.), *Learning
and computational neuroscience* (pp. 497–537). MIT Press.

------------------------------------------------------------------------

1.  A’s fourth compound and B’s second compound.

2.  This can be understood as the expected value if the expected
    stimulus has some reward value.
