births <- read.csv("chds_births.csv")





#temporary models -- change this
M1 <- lm(wt ~ gestation + parity + time + number  + smoke + marital + fed + med, data=birth.data)
M2 <- lm(wt ~ gestation + parity + smoke + marital + fed, data=birth.data)

print(M2$call)
train.ind <- sample(ntot, ntrain)

# Ideas for Initial Model

## Variables to include
\begin{enumerate}
\item gestation
\item parity
\item time
\item number
\item smoke
\item martial
\item fed
\item med
\end{enumerate}

## Variables to consider including
\begin{enumerate}
\item meth
\item feth
\item mage/fage (not both -- correlated)
\item mht/mwt (slightly correlated so probably not both -- mwt might be better)
\end{enumerate}

## Non-linear effects/ other modifications to covariates
\begin{itemize}
\item change grouping of smoke: group 0 and 3 together; 1 and 2 together to form "never/used to" and "now/until pregnancy"
\item change grouping of med/fed: (0, 1, 7) becomes group "no highschool/ highschool unclear", (3, 6) -> trade, (4, 5) -> college [the latter 2 groupings are relevant for fed more than med]
\item income* (have to fix with imputation first)
\end{itemize}