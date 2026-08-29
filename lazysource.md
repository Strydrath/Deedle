# Delay-loaded series

The `DelayedSeries` type provides an efficient way to create series whose data is loaded
on-demand. For example, you may have a large time series stored in a CSV file or in a
database and you do not want to load all the data in memory if the user only needs a
small part of it.

When you create a delayed series, you specify the overall range of the series (i.e. the
minimum and maximum key value) and you provide a function that loads a specified sub-range
of the series. When the user accesses a continuous range of the series, the loading function
is called to retrieve the data.

<a name="create"></a>
## Creating a delayed series

To create a delayed series, we need a function that generates data for a given range.
The following function generates a series with random data for a given date range with
a day frequency:

```fsharp
let generate (low:DateTime) (high:DateTime) : seq<KeyValuePair<DateTime,float>> = 
    let rnd = Random()
    let days = int (high - low).TotalDays
    seq [ for d in 0 .. days -> KeyValuePair(low.AddDays(float d), rnd.NextDouble()) ]
```

Now we use `DelayedSeries.FromValueLoader` to create a delayed series. It takes the overall
minimum and maximum key of the series and a function that loads data for a sub-range. The
loading function gets the lower and upper bound as a tuple of `(key, BoundaryBehavior)`
values where `BoundaryBehavior` is either `Inclusive` or `Exclusive`:

```fsharp
let min = DateTime(2010, 1, 1)
let max = DateTime(2013, 1, 1)

let ls = DelayedSeries.FromValueLoader(min, max, fun (lo, lob) (hi, hib) -> async {
    printfn "Query: %A - %A" lo hi
    let lo = if lob = BoundaryBehavior.Inclusive then lo else lo.AddDays(1.0)
    let hi = if hib = BoundaryBehavior.Inclusive then hi else hi.AddDays(-1.0)
    return generate lo hi })
```

The key thing about the above is that, so far, no data has been loaded. The loading function
is called only when we access part of the series.

<a name="slicing"></a>
## Slicing and using delayed series

We can now use the series as usual - for example, to get data for the entire year 2012:

```fsharp
let slice = ls.[DateTime(2012, 1, 1) .. DateTime(2012, 12, 31)]
slice
```

```
val slice: Series<DateTime,float> =
  
(Delayed series [01/01/2012 .. 12/31/2012]) 

val it: Series<DateTime,float> =
  
(Delayed series [01/01/2012 .. 12/31/2012])
```

Similarly, we can add the delayed series to a data frame. When doing this, Deedle will
only load the data that is needed. In the following example, we add the series to a frame
and then access only a slice:

```fsharp
let df = frame ["Values" => ls]
let slicedDf = df.Rows.[DateTime(2012,6,1) .. DateTime(2012,6,30)]
slicedDf
```

```
Query: 01/01/2010 00:00:00 - 01/01/2013 00:00:00
Query: 06/01/2012 00:00:00 - 06/30/2012 00:00:00
val df: Frame<DateTime,string> =
  
              Values               
01/01/2010 -> 0.4031565348732864   
01/02/2010 -> 0.3305455708719143   
01/03/2010 -> 0.07146760531583396  
01/04/2010 -> 0.7545245584461446   
01/05/2010 -> 0.4777489525923774   
01/06/2010 -> 0.751466094714169    
01/07/2010 -> 0.07581910605991293  
01/08/2010 -> 0.02561957555479799  
01/09/2010 -> 0.3190096166403622   
01/10/2010 -> 0.9956554292502      
01/11/2010 -> 0.1624466065986565   
01/12/2010 -> 0.2513435666354965   
01/13/2010 -> 0.3190899229607339   
01/14/2010 -> 0.7134733788207331   
01/15/2010 -> 0.3225283036971831   
:             ...                  
12/18/2012 -> 0.7082947764559312   
12/19/2012 -> 0.8385245150319378   
12/20/2012 -> 0.44425861144150325  
12/21/2012 -> 0.9696677106331977   
12/22/2012 -> 0.6825419612180396   
12/23/2012 -> 0.1847696869535912   
12/24/2012 -> 0.40246518252086616  
12/25/2012 -> 0.5936166276348592   
12/26/2012 -> 0.6084608702808493   
12/27/2012 -> 0.11888851895819819  
12/28/2012 -> 0.009929706587315623 
12/29/2012 -> 0.5369909782539847   
12/30/2012 -> 0.4386656699344146   
12/31/2012 -> 0.5431025608699396   
01/01/2013 -> 0.04189845282058946  

val slicedDf: Frame<DateTime,string> =
  
              Values              
06/01/2012 -> 0.05567085416468098 
06/02/2012 -> 0.5165788619845635  
06/03/2012 -> 0.476645491383915   
06/04/2012 -> 0.3186668211906669  
06/05/2012 -> 0.32650455487940777 
06/06/2012 -> 0.8983276337531761  
06/07/2012 -> 0.5993533294044452  
06/08/2012 -> 0.957669766026409   
06/09/2012 -> 0.4801687804132492  
06/10/2012 -> 0.5026459875854423  
06/11/2012 -> 0.46188253788461664 
06/12/2012 -> 0.4247301693090366  
06/13/2012 -> 0.06370799012756745 
06/14/2012 -> 0.6596978246293657  
06/15/2012 -> 0.1166683624598277  
06/16/2012 -> 0.9466184382522341  
06/17/2012 -> 0.07846451563983126 
06/18/2012 -> 0.6854589233947688  
06/19/2012 -> 0.30084360216340655 
06/20/2012 -> 0.6218939784907178  
06/21/2012 -> 0.9022728345366783  
06/22/2012 -> 0.06281098848292865 
06/23/2012 -> 0.3395361466086635  
06/24/2012 -> 0.5722276844986165  
06/25/2012 -> 0.7499479258575513  
06/26/2012 -> 0.9344069967060159  
06/27/2012 -> 0.5895799997661823  
06/28/2012 -> 0.7080857221427513  
06/29/2012 -> 0.42820634337570995 
06/30/2012 -> 0.15504896481878272 

val it: Frame<DateTime,string> =
  
              Values              
06/01/2012 -> 0.05567085416468098 
06/02/2012 -> 0.5165788619845635  
06/03/2012 -> 0.476645491383915   
06/04/2012 -> 0.3186668211906669  
06/05/2012 -> 0.32650455487940777 
06/06/2012 -> 0.8983276337531761  
06/07/2012 -> 0.5993533294044452  
06/08/2012 -> 0.957669766026409   
06/09/2012 -> 0.4801687804132492  
06/10/2012 -> 0.5026459875854423  
06/11/2012 -> 0.46188253788461664 
06/12/2012 -> 0.4247301693090366  
06/13/2012 -> 0.06370799012756745 
06/14/2012 -> 0.6596978246293657  
06/15/2012 -> 0.1166683624598277  
06/16/2012 -> 0.9466184382522341  
06/17/2012 -> 0.07846451563983126 
06/18/2012 -> 0.6854589233947688  
06/19/2012 -> 0.30084360216340655 
06/20/2012 -> 0.6218939784907178  
06/21/2012 -> 0.9022728345366783  
06/22/2012 -> 0.06281098848292865 
06/23/2012 -> 0.3395361466086635  
06/24/2012 -> 0.5722276844986165  
06/25/2012 -> 0.7499479258575513  
06/26/2012 -> 0.9344069967060159  
06/27/2012 -> 0.5895799997661823  
06/28/2012 -> 0.7080857221427513  
06/29/2012 -> 0.42820634337570995 
06/30/2012 -> 0.15504896481878272
```
