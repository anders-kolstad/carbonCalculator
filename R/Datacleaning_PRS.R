PRS <- PRS_results[-1,]

library("ggplot2")
library("gridExtra")

pNO3.N <- ggplot(PRS, aes(x=Treatment, y=NO3.N)) +
  geom_boxplot()
pNO3.N
pNH4.N <-ggplot(PRS, aes(x=Treatment, y=NH4.N)) +
  geom_boxplot()
pNH4.N
pCa <-ggplot(PRS, aes(x=Treatment, y=Ca)) +
  geom_boxplot()
pCa
pMg <-ggplot(PRS, aes(x=Treatment, y=Mg)) +
  geom_boxplot()
pMg
pK <-ggplot(PRS, aes(x=Treatment, y=K)) +
  geom_boxplot()
pK
pP <-ggplot(PRS, aes(x=Treatment, y=P)) +
  geom_boxplot()
pP
pFe <-ggplot(PRS, aes(x=Treatment, y=Fe)) +
  geom_boxplot()
pFe
pMn <-ggplot(PRS, aes(x=Treatment, y=Mn)) +
  geom_boxplot()
pMn
pCu <-ggplot(PRS, aes(x=Treatment, y=Cu)) +
  geom_boxplot()
pCu
pZn <-ggplot(PRS, aes(x=Treatment, y=Zn)) +
  geom_boxplot()
pZn
pB <-ggplot(PRS, aes(x=Treatment, y=B)) +
  geom_boxplot()
pB
pS <-ggplot(PRS, aes(x=Treatment, y=S)) +
  geom_boxplot()
pS
pPb <-ggplot(PRS, aes(x=Treatment, y=Pb)) +
  geom_boxplot()
pPb
pAl <-ggplot(PRS, aes(x=Treatment, y=Al)) +
  geom_boxplot()
pAl
pCd <-ggplot(PRS, aes(x=Treatment, y=Cd)) +
  geom_boxplot()
pCd

grid.arrange(pNO3.N, pNH4.N, pCa, pMg, pK, pP, pFe, pMn, pCu,
             pZn, pB, pS, pPb, pAl, pCd)