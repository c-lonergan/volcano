# volcano
Microarray Data: Interactive Volcano Plot

Allows the user to explore and visualise the results of a differential expression analysis using R Shiny.

 - The data is read in using a reactive function.
 - The desired number of genes (specified by a numeric input field) is displayed in a table, sorted by significance.
 - Sliders allow the user to set thresholds for adjusted p-values and logFC values. 
 - A volcano plot showing the thresholds set and highlighting the differentially expressed genes at those thresholds is be displayed. 
 - The thresholds set and number of differentially expressed genes are shown in the plot title.
 - A summary table displayes useful statistics of highlighted genes.

![volcano](https://user-images.githubusercontent.com/72213939/136067249-02f3311c-cdb3-4b03-aea1-68870aa73664.png)
