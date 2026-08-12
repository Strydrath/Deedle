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
01/01/2010 -> 0.13475891576355803 
01/02/2010 -> 0.41605890750313923 
01/03/2010 -> 0.2634801032019929  
01/04/2010 -> 0.6235223270106461  
01/05/2010 -> 0.2665507230166778  
01/06/2010 -> 0.32310779393364975 
01/07/2010 -> 0.4271504863091181  
01/08/2010 -> 0.9790888345431956  
01/09/2010 -> 0.06371933797075846 
01/10/2010 -> 0.8227473718688106  
01/11/2010 -> 0.45307384659905736 
01/12/2010 -> 0.8182922874407775  
01/13/2010 -> 0.3709578769617312  
01/14/2010 -> 0.451526145603827   
01/15/2010 -> 0.7498778278364235  
:             ...                 
12/18/2012 -> 0.7145971078767044  
12/19/2012 -> 0.3555790450522297  
12/20/2012 -> 0.07925626108274975 
12/21/2012 -> 0.13850140332408545 
12/22/2012 -> 0.15272141731949151 
12/23/2012 -> 0.2997300639170304  
12/24/2012 -> 0.40364727000659995 
12/25/2012 -> 0.5688222652768299  
12/26/2012 -> 0.3943726392146909  
12/27/2012 -> 0.6319753284778181  
12/28/2012 -> 0.810381962114077   
12/29/2012 -> 0.03120883647043282 
12/30/2012 -> 0.42169962993724297 
12/31/2012 -> 0.0876254514789726  
01/01/2013 -> 0.09528132490141628 

val slicedDf: Frame<DateTime,string> =
  
              Values               
06/01/2012 -> 0.29771161851845473  
06/02/2012 -> 0.8964211247179619   
06/03/2012 -> 0.6560821740953438   
06/04/2012 -> 0.8065800262732841   
06/05/2012 -> 0.1559848865901251   
06/06/2012 -> 0.8196148381588352   
06/07/2012 -> 0.4250741162164643   
06/08/2012 -> 0.6343035336268369   
06/09/2012 -> 0.42351511223960525  
06/10/2012 -> 0.5745722367277837   
06/11/2012 -> 0.1718429950433713   
06/12/2012 -> 0.054972393282713194 
06/13/2012 -> 0.9100839806370477   
06/14/2012 -> 0.7937097652438282   
06/15/2012 -> 0.5501208869353321   
06/16/2012 -> 0.8423335371380078   
06/17/2012 -> 0.5287332111009329   
06/18/2012 -> 0.421561219605075    
06/19/2012 -> 0.5817306327341936   
06/20/2012 -> 0.0902174125855758   
06/21/2012 -> 0.4568540213302422   
06/22/2012 -> 0.7388206987236456   
06/23/2012 -> 0.5184910933373801   
06/24/2012 -> 0.6716401924995573   
06/25/2012 -> 0.21909334437009031  
06/26/2012 -> 0.9757630212922864   
06/27/2012 -> 0.43033792436933294  
06/28/2012 -> 0.21541845710991014  
06/29/2012 -> 0.24142606437625747  
06/30/2012 -> 0.5430077312783955   

val it: Frame<DateTime,string> =
  
              Values               
06/01/2012 -> 0.29771161851845473  
06/02/2012 -> 0.8964211247179619   
06/03/2012 -> 0.6560821740953438   
06/04/2012 -> 0.8065800262732841   
06/05/2012 -> 0.1559848865901251   
06/06/2012 -> 0.8196148381588352   
06/07/2012 -> 0.4250741162164643   
06/08/2012 -> 0.6343035336268369   
06/09/2012 -> 0.42351511223960525  
06/10/2012 -> 0.5745722367277837   
06/11/2012 -> 0.1718429950433713   
06/12/2012 -> 0.054972393282713194 
06/13/2012 -> 0.9100839806370477   
06/14/2012 -> 0.7937097652438282   
06/15/2012 -> 0.5501208869353321   
06/16/2012 -> 0.8423335371380078   
06/17/2012 -> 0.5287332111009329   
06/18/2012 -> 0.421561219605075    
06/19/2012 -> 0.5817306327341936   
06/20/2012 -> 0.0902174125855758   
06/21/2012 -> 0.4568540213302422   
06/22/2012 -> 0.7388206987236456   
06/23/2012 -> 0.5184910933373801   
06/24/2012 -> 0.6716401924995573   
06/25/2012 -> 0.21909334437009031  
06/26/2012 -> 0.9757630212922864   
06/27/2012 -> 0.43033792436933294  
06/28/2012 -> 0.21541845710991014  
06/29/2012 -> 0.24142606437625747  
06/30/2012 -> 0.5430077312783955
```
