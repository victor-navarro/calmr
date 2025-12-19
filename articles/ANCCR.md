# ANCCR

## The mathematics behind ANCCR

The ANCCR (Jeong et al., 2022) model, which stands for adjusted net
contingency for causal relations, proposes that mesolimbic dopaminergic
conveys an adjusted net contingency for causal relationships (to
biologically meaningful targets). The mathematics (and logic) behind the
model go well beyond what I can cover here, but for now, it will suffice
to say that the model:

- Uses a “Hebbian” mechanism to learn retrospective associations after
  experiencing a meaningful causal target.
- Derives prospective associations using Bayes’s rule.
- Combines those associations into contingency terms that represent
  dopaminergic activity.
- Uses the sign of dopaminergic activity to strengthen or weaken causal
  weights.
- Responds as a function of prospective associations and causal links.

### 1 - Maintaining stimulus representations

The degree to which a stimulus $i$ at time $t$ is “active” in memory is
denoted by:

$$E_{i}(t) = \Sigma_{t_{i} \leq t}e^{- {(\frac{t - t_{i}}{t\_ constant})}}$$

where $t_{i}$ are all the time steps up to time $t$, and $t\_ constant$
is a time constant (usually meant to be the inter-reward rate)[¹](#fn1)

### 2 - Learning stimulus associations:

The model learns retrospective associations after meaningful causal
targets occur. Whether event $j$ is a meaningful causal target is given
by:

$$\Phi_{j} = \begin{cases}
{1,} & {{\text{if}\mspace{6mu}}\Phi_{j}(t) = 1} \\
{1,} & {{\text{if}\mspace{6mu}}DA_{j} + \beta_{j} > \theta} \\
{0,} & \text{otherwise}
\end{cases}$$

where $\Phi$ plays the role of an indicator function, $DA_{j}$ is the
total dopamine activity at the time of event $j$, $\beta_{j}$ is the
unconditioned value of event $j$ and $\theta$ is a global threshold
parameter.[²](#fn2) Note that the indicator function is self-preserving:
once a stimulus becomes a meaningful causal target, it does not stop
being so.

After stimulus $j$ is observed, the predecessor representation
contingency, or **PRC**, for each stimulus $i$ is updated via:

$$PRC_{i\leftarrow j} = M_{i\leftarrow j} - M_{i}$$

where $M_{i\leftarrow j}$ is the predecessor representation of $i$ given
$j$ has occurred, and $M_{i}$ is the base rate with which $i$ occurs.
Both of these quantities are given by:

$$M_{i\leftarrow j} = M_{i\leftarrow j}\prime + \Phi_{j}\alpha\left( E_{i\leftarrow j} - M_{i\leftarrow j}\prime \right)$$

and

$$M_{i} = M_{i}\prime + k\alpha\left( E_{i} - M_{i}\prime \right)$$

where $M_{i\leftarrow j}\prime$ and $M_{i}\prime$ are the quantities
before $j$ was observed, $k$ and $\alpha$ are learning rate parameters,
and $E_{i\leftarrow j}$ is the eligibility trace of stimulus $i$ at the
time $j$ occurs (see Eq. 1).

Then, the PRC can be used to derive the prospective association, aptly
named the successor representation contingency, or **SRC** via Bayes
rule:

$$SRC_{i\rightarrow j} = PRC_{i\leftarrow j}\frac{M_{j}}{M_{i}}$$

The base rate for $j$, $M_{j}$ is calculated via Eq.4b.

### 3 - Releasing Dopamine

The model postulates that dopaminergic signaling encodes the adjusted
net contingencies for causal relations between stimuli, or ANCCRs. The
total dopaminergic activity at the time of event $i$ is equal to:

$$DA_{i} = \Sigma_{j}\left( ANCCR_{i\rightarrow j}\Phi_{j} \right)$$

And the ANCCR from stimulus $i$ to stimulus $j$ is given by:

$$ANCCR_{i\rightarrow j} = NC_{i\leftrightarrow j}CW_{i\rightarrow j} - \sum\limits_{k \neq i}\left( ANCCR_{k\leftrightarrow j}\Delta_{k\leftarrow i}\Phi_{k\leftrightarrow i} \right)$$

where $NC_{i\leftrightarrow j}$ is the net contingency between stimuli
$i$ and $j$, $CW_{i\rightarrow j}$ is the causal weight that $i$ has
with $j$, $\Delta_{k\leftarrow i}$ is the recency of stimulus $k$ with
respect to stimulus $i$, and $\Phi_{k\leftrightarrow i}$ is an indicator
function denoting whether $k$ and $i$ have a putative causal
relationship with each other.

The net contingency between stimuli $i$ and $j$,
$NC_{i\leftrightarrow j}$, is given by:

$$NC_{i\leftrightarrow j} = wSRC_{i\rightarrow j} + (1 - w)PRC_{i\leftarrow j}$$

or a weighted sum of successor and predecessor representation
contingencies.

The net contingency is used to calculate the indicator function above,
as:

$$\Phi_{k\leftrightarrow i} = \begin{cases}
{1,} & {{\text{if}\mspace{6mu}}NC_{i\leftrightarrow j} > \theta} \\
{0,} & \text{otherwise}
\end{cases}$$

where $\theta$ is the same threshold parameter used in Eq.2[³](#fn3),
and the indicator function for a stimulus and itself,
$\Phi_{i\leftrightarrow i}$, is 0.

The recency term, $\Delta_{k\leftarrow i}$, is given by:

$$\Delta_{k\leftarrow i} = e^{- {(\frac{t_{j} - t_{i}}{t\_ constant})}}$$

where $t\_ constant$ is the same parameter used in Eq.1. Note however
that Eq.9 does not include the sum term in Eq. 1. Finally, the causal
weight from stimulus $i$ to stimulus $j$ is given by:

$$CW_{i\rightarrow j} = CW_{i\rightarrow j}\prime + \alpha_{reward}\delta_{i\rightarrow j}$$

where $CW_{i\rightarrow j}\prime$ is the previous causal weight,
$\alpha_{reward}$ is a learning rate parameter exclusive for causal
weights, and $\delta_{i\rightarrow j}$ is a delta term depending on the
sign of the total dopaminergic activity, given by:

$$\delta_{i\rightarrow j} = \begin{cases}
{CW_{j\rightarrow j} - CW_{i\rightarrow j},} & {{\text{if}\mspace{6mu}}DA_{j} \geq 0} \\
{\left( 0 - CW_{i\rightarrow j} \right)\frac{n_{i}^{- 1}\Delta\left. i\leftarrow j \right.\Phi_{i\leftrightarrow j}}{\Sigma_{k \neq j}\left( n_{k}^{- 1}\Delta_{k\leftarrow j}\Phi_{k\leftrightarrow j} \right)},} & \text{otherwise}
\end{cases}$$

where $CW_{j\rightarrow j}$ above is the reward magnitude of stimulus
$j$. In plain words, when dopaminergic activity is positive, causal
weights (from all present and absent stimuli) strengthen. Conversely,
when dopaminergic activity is negative, causal weights (from all present
and absent stimuli) weaken, proportional to their normalized frequency
and recency (as long as they have putative causal relations with $j$).

### 4 - Generating responses

Responding in ANCCR is lightly specified. The value of responding upon
presentation of stimulus $i$ is given by:

$$Q_{i} = \Sigma_{k}\left( SRC_{i\rightarrow k}CW_{i\rightarrow k} \right)$$

which can then be mapped onto probabilities via a softmax
function[⁴](#fn4).

##### A diagram

The diagram below shows the dependencies in the model. I am excluding
the indicator functions and parameters for simplicity.[⁵](#fn5)

##### Note

The implementation of this model is a port from the MATLAB code that
Jeong et al. shared in the [GitHub
repository](https://github.com/namboodirilab/ANCCR) associated with
their paper. The output of the R model was checked against the outputs
of the MATLAB model, using training routines (“eventlogs” in their
parlance) generated using their MATLAB code. The training routines
generated in `calmr` differ somewhat, to accommodate generality. For
example, as of version `0.6.1`, it is not possible to specify
probabilistic relations between cues and rewards. Instead, it is left to
the user to specify an exact probability via trial numbers (e.g., an 80%
reward probability can be specified as “80A\>(US)/20A”). The naming of
parameters also differs between codebases.

##### References

Jeong, H., Taylor, A., Floeder, J. R., Lohmann, M., Mihalas, S., Wu, B.,
Zhou, M., Burke, D. A., & Namboodiri, V. M. K. (2022). Mesolimbic
dopamine release conveys causal associations. *Science (New York,
N.Y.)*, *378*, eabq6740. <https://doi.org/10.1126/science.abq6740>

------------------------------------------------------------------------

1.  The $t\_ constant$ defaults to `NA`, and if it remains like that, is
    calculated internally as the mean of the mean ITI for each trial.
    You should set this by hand if only some of your trials contain
    rewards.

2.  The first case in Eq. 2 was not written in the supplementary
    information of the Jeong et al. paper, but it reflects their MATLAB
    implementation.

3.  Notably, although Eq.2 is self-perpetuating, Eq.9 is not, meaning
    causal relations between stimuli can flip between 0 and 1

4.  The implementation of ANCCR in `calmr` returns both action values
    and probability matrices. The latter is parametrized via the global
    parameters $cost$ (the cost of responding) and $temperature$, a
    multiplier, **not divider** on action values

5.  If nothing appears below, try rebuilding this vignette after install
    `DiagrammeR`
