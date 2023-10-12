# ECS 132 Notes

[TOC]

## Mentioned Exampl

### ALOHA Network

### Bus Ridership

### Preferential Attachment Matrix

### Dice Problem

## Formulas, Equations, Rules

*Lower case letter $c$ will repsresent a constant*

### Basic Rules

If events are independent:
$$
P(A\ or\ B) = P(A)+P(B)\\
P(A\ and\ B) = P(A) \cdot P(B)\\
p(A and B) = P(A)P(B|A)\\
P(B|A) = {P(A\ and\ B) \over P(A)}\\
$$

### Bayes’ Rule

$$
P(A|B) = {P(A)P(B|A) \over P(A)P(B|A) + P(not\ A)P(B|not\ A)}
$$

### Expected Value

$$
E(X) = \sum _{c \in A} c P(X = c)\\
E(U+V) = E(U) + E(V)\\
E(cU) = c\cdot EU\\
E(UV) = EU \cdot EV\\
E[g(X)] = \sum _{c\in A} g(c) \cdot P(X=c)
$$

### Variance

$$
Var(U) = E[(U - EU)^2] \tag{Variance Definition}
$$

$$
Var(U) = E(U^2) - (EU)^2\\
Var(cU) =c^2Var(U)\\
Var(U+c) = Var(U)\\
\text{coef. of var.} = {\sqrt{Var(X)} \over EX}
$$

### Covariance

$$
Cov(U, V) = E[(U-EU)(V-EV)]\\
Cov(U, V) = E(UV) - EU \cdot EV
$$



## Distribution

The **Probability Mass Function** (pmf) of a discrete random variable V, denoted $p_{V}$, as
$$
p_{V}(k) = P(V = k)
$$

### Example: Toss Coin Until First Head

$$
p_{N}(k) = {1 \over 2^k}, k=1,2, ...
$$

### Parameteric Families of pmfs





### Notations

N choose M notation: $N \choose M$ This represetn how many combinations are there to choose M elements from N elemetns.

Eg:

68 students, among them, 48 cs students, choose 4 from 68 that are exactly 2 cs to other major: ${48 \choose 2} {20 \choose 2} \over {68 \choose 4}$
