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
01/01/2010 -> 0.9251557214504038  
01/02/2010 -> 0.6717506076428417  
01/03/2010 -> 0.8815975782006292  
01/04/2010 -> 0.5838383947337515  
01/05/2010 -> 0.44631019510479386 
01/06/2010 -> 0.3314954598244175  
01/07/2010 -> 0.04924858378390684 
01/08/2010 -> 0.9348069594299447  
01/09/2010 -> 0.626904941536306   
01/10/2010 -> 0.8050041589090887  
01/11/2010 -> 0.3326077686568043  
01/12/2010 -> 0.9209428880384446  
01/13/2010 -> 0.48519755014764787 
01/14/2010 -> 0.4837716009362757  
01/15/2010 -> 0.3050512958994165  
:             ...                 
12/18/2012 -> 0.46512880079591057 
12/19/2012 -> 0.05619607729124598 
12/20/2012 -> 0.8900618486550211  
12/21/2012 -> 0.8192605311016331  
12/22/2012 -> 0.6149430629122082  
12/23/2012 -> 0.4131643757167742  
12/24/2012 -> 0.2616525052278691  
12/25/2012 -> 0.7434048340168109  
12/26/2012 -> 0.445763263549798   
12/27/2012 -> 0.9291405030956945  
12/28/2012 -> 0.08889070158513379 
12/29/2012 -> 0.8720135664208847  
12/30/2012 -> 0.7019259103710167  
12/31/2012 -> 0.8253080252391111  
01/01/2013 -> 0.7111018107119896  

val slicedDf: Frame<DateTime,string> =
  
              Values               
06/01/2012 -> 0.6872798368122113   
06/02/2012 -> 0.39523955736193495  
06/03/2012 -> 0.20761836140508017  
06/04/2012 -> 0.6213950712558369   
06/05/2012 -> 0.33536896148019546  
06/06/2012 -> 0.29503064595838346  
06/07/2012 -> 0.290423399268528    
06/08/2012 -> 0.5702665001671338   
06/09/2012 -> 0.8536122378475512   
06/10/2012 -> 0.9191274039916403   
06/11/2012 -> 0.26092087965640975  
06/12/2012 -> 0.5722480179025746   
06/13/2012 -> 0.9446140985199992   
06/14/2012 -> 0.25773343570178     
06/15/2012 -> 0.5120763519611033   
06/16/2012 -> 0.044772885288638875 
06/17/2012 -> 0.8237752282933143   
06/18/2012 -> 0.7616391825765104   
06/19/2012 -> 0.14044350189699406  
06/20/2012 -> 0.10506225956526216  
06/21/2012 -> 0.6715781088372874   
06/22/2012 -> 0.2595696528028334   
06/23/2012 -> 0.7375207412639727   
06/24/2012 -> 0.5847945842675736   
06/25/2012 -> 0.7695010170844365   
06/26/2012 -> 0.8155838817216643   
06/27/2012 -> 0.3181329622633393   
06/28/2012 -> 0.15064837482642723  
06/29/2012 -> 0.268772233933547    
06/30/2012 -> 0.7603361166261886   

val it: Frame<DateTime,string> =
  
              Values               
06/01/2012 -> 0.6872798368122113   
06/02/2012 -> 0.39523955736193495  
06/03/2012 -> 0.20761836140508017  
06/04/2012 -> 0.6213950712558369   
06/05/2012 -> 0.33536896148019546  
06/06/2012 -> 0.29503064595838346  
06/07/2012 -> 0.290423399268528    
06/08/2012 -> 0.5702665001671338   
06/09/2012 -> 0.8536122378475512   
06/10/2012 -> 0.9191274039916403   
06/11/2012 -> 0.26092087965640975  
06/12/2012 -> 0.5722480179025746   
06/13/2012 -> 0.9446140985199992   
06/14/2012 -> 0.25773343570178     
06/15/2012 -> 0.5120763519611033   
06/16/2012 -> 0.044772885288638875 
06/17/2012 -> 0.8237752282933143   
06/18/2012 -> 0.7616391825765104   
06/19/2012 -> 0.14044350189699406  
06/20/2012 -> 0.10506225956526216  
06/21/2012 -> 0.6715781088372874   
06/22/2012 -> 0.2595696528028334   
06/23/2012 -> 0.7375207412639727   
06/24/2012 -> 0.5847945842675736   
06/25/2012 -> 0.7695010170844365   
06/26/2012 -> 0.8155838817216643   
06/27/2012 -> 0.3181329622633393   
06/28/2012 -> 0.15064837482642723  
06/29/2012 -> 0.268772233933547    
06/30/2012 -> 0.7603361166261886
```
