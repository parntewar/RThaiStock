## About
RthaiStock is a project that use [R](https://www.r-project.org/) and [Shiny](https://shiny.rstudio.com/) to help users manage theirs portfolio. 
It provide an easy one click solution for everyday use.
It only focus on Thai stock and still in the early stage. Use it with cautious.

## Method
RthaiStock uses technical analysis and some statistic values to decide what portfolio will be.
Currently, there is only one method use and some value can alter by edit in code.

## How to use
It design to use daily and once the market is close(after 16.45 PM to 17.25 PM UTC + 7)
In report folder. Run run_shiny.R file. 
Shiny window will pop-up. Click update data button. It will take around 5 to 6 minutes. 
After it done trade report tab will update and report which stock should buy or sell.
If nothing show up go to cash balance tab and update how much cash you have to buy stocks(in Baht).
In trade report tab it will show what stocks to buy(or sell) in the next day. 
What shows there will filter by cash balance, stock price and favor value(a statistic value) threshold.
Click download orders button. It will write xls file in main folder. This file can use with streaming platform in multi-order menu to buy(or sell) stocks.
After orders match in next day return to modify this file by adding new column about how much (in Baht) buy or sell then click update order buttons.
