---
title: "Climate"
author: "KO"
date: "02/09/2021"
output: html_document
---

# Sample climate report

CURRENT STAGE: Aggregated inputs (ordered by materiality)


```r
print(AggregatedTypeInputs())
```

```
##       item values
## 3 property      H
## 1     coal      M
## 2      gas      M
```


TO DO: implement the following mechanics
```
      for scenario in (2.5,4)
        for item in AggregatedTypeInputs
          if(scenario==2.5)
            text(item,physical,low,always)
            if(item.materiality=='H')
              text(item,physical,low,extra)
            text(item,transition,high,always)
            if(item.materiality=='H')
              text(item,transition,high,extra)
          else
            text(item,physical,high)
            if(item.materiality=='H')
              text(item,physical,low,extra)
```

the structure may be organised using child documents


## Details from the child document

Hi, there. I'm a child with as many details as necessary
