# Strategy
## Preprocessing
- Þreifa áfram eftir back selection
- Tékka á Stepwise xx f. samanburð seinna
- Bonferroni leiðrétting - ítrekaðar tilraunir?
- TukeyHSD (Bara í ANCOVA?)
- ef f(y) --> f(r_y)^-1 
- Þurfum við sjálfir að þýða categorical --> numerical f. ANCOVA?

## Diagnostics
- $R^2$ adjusted plot?
	- þarf ákvörðun um xx röð skýribreyta
- PRESS
	- auðvelt samanb. tól
- Cp stat
	- greinarbetra samanb. tól
- AIC
- Colinearity (ggpairs)
- Samspil ANCOVA og lin. regression
	- ekki vandamál? (hvort tveggja línulegt)
	- greina aðskilið?

## Testing
- Ítra yfir test set
	- bootstrap?
	- mean RMSE yfir margar ítranir?
