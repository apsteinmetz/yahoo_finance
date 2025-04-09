
::: {.card title="Cumulative Risk"} 
```{r}
cum_risk
```
:::
   
   :::{.card title="Risk Decomposition"}
```{r}
risk_decomp
```
:::
   ###
   
   ### row
   :::{.card title="Rolling Risk"}
![](data/rolling_risk.gif)
:::
   ###
   ##
   
   
   :::
   :::: {.panel-tabset}
## Cumulative Growth
```{r}
mountain
```
## LTM Cumulative Growth
```{r}
mountain_ltm
```
::::
   :::
   
   ```{r}
#| content: "valuebox"
#| title: "As Of:"
vbs[[4]]
```
```{r}
#| content: "valuebox"
#| title: "Total Value"
vbs[[1]]
```
```{r}
#| content: "valuebox"
#| title: "Latest Volatility"
vbs[[2]]

```{r}
#| content: "valuebox"
#| title: "Historical Volatility"
vbs[[3]]
```

create a quarto block layout with three columns, 3 rows in the first column, 2 rows in the second and third columns