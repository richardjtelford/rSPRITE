# rSPRITE 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

# Version history

* 2018-02-19 16:08 UTC 0.01
*   First Shiny version released.
* 2018-02-19 17:23 UTC 0.02
*   Improved the look of the X-axis (numbers can go sideways if they start to bunch up).
*   Added an upper limit for the X-axis for when no items get close to the scale maximum.
*   Added code to use human-friendly intervals on the Y-axis.
* 2018-02-19 18:31 UTC 0.03
*   Fixed a bug that caused a previous error/warning message to hang around on the next run.
*   Added version number display
* 2018-02-19 21:43 UTC 0.04
*   Added input elements to allow a specific response value to appear a fixed number of times.
* 2018-02-20 17:21 UTC 0.05
*   Fixed a bug that caused a crash 50% of the time when scaleMin and scaleMax were both negative.
*   Added dynamic updates of upper/lower bounds of input controls, depending on the values of others.
* 2018-02-21 15:07 UTC 0.06
*   Fixed a bug that caused spurious error messages with certain fixed-value configurations.
*   Plots now appear sorted from smallest to largest skewness.
*   Added a rudimentary help feature.
*   Fixed a bug that meant that the user couldn't easily type in a SD close to the permitted minimum.
*   Fixed a bug that could cause errors in extreme cases with delta=2 in rSprite.delta.
*   Improved performance by taking a pragmatic decision about when to stop looking for duplicates.
* 2018-02-21 23:22 UTC 0.07
*   Added link to enable user to download the data that went into the plots.
*   Fixed a bug that was preventing solutions from being found with very large means and very small SDs.
* 2018-03-03 23:46 UTC 0.08
*   Increased the size of the plot area.
*   Increased maximum grid size to 10 x 10.
*   Changed plot bar colour for better visibility if black text encroaches on bars.
*   Reduced the chances of missing a valid solution when only a few (requested number > n > 1) exist.
*   Changed displayed name to rSprite.
* 2018-03-24 15:00 UTC 0.09
*   Display solutions on the smallest grid that they will fit onto.
*   User now chooses the number of results they want, not the grid size.
*   Moved the decimal places input field to just below the mean and SD.
*   Fixed a bug that could cause spurious solutions to be returned if none were possible.
* 2018-03-27 20:23 UTC 0.10
*   Fixed a bug that could cause the "No solution found" message to be split into two.
*   Fixed a bug that prevented entering 0 or a negative number as the fixed value.
*   Fixed a bug that prevented a solution from being found in some extreme circumstances.
*   Fixed a bug that produced variable bar widths with large X-axis ranges.
* 2018-04-18 13:50 UTC 0.11
*   Fixed a bug that prevented the SD granularity from being changed.
*   Tightened the restrictions on the maximum SD that can be entered.
*   Moved the scale limit fields to the top of the list.
*   Fixed a small bug that sometimes showed more ticks than necessary on the X-axis.
*   Allow fixed values to be outside the scale range.
*   Changed displayed name to rSPRITE.
* 2018-05-22 13:32 UTC 0.12
*   Fixed a bug that caused a failure to calculate the possible SD range in some extreme samples.
* 2018-05-26 19:27 UTC 0.13
*   Added note about privatcy to the help text.
*   Added blank line before download link.
*   Added "loading" spinner image.
* 2018-11-08 23:40 UTC 0.14
*   Increased the size of the plot area.
*   Changed help text to point to preprint article instead of James's blog post.
*   Added CC-BY license.
*   Fixed a small bug that caused slightly different X-axis widths depending on the data.
* 2019-06-02 20:56 UTC 0.15
*   Fixed a bug that could cause valid SDs to be rejected as too small with means near the scale limits.
* 2020-06-23 21:34 UTC 0.16
*   Increased maximum sample size to 10000.
*   Fixed a bug that could make it impossible to enter a target SD in a small number of cases.
*   Added code to make input fields slightly less reactive.
*   I am trying two ways to avoid the Shiny reactivity issue whereby with, for example,
   scale value min=3 and max=50, the user wants to type 25 for the mean but the 2 immediately gets changed to 3.
*   limits: include/exclude these lines to toggle automatic checking of mean/SD limits
*   bounce: include/exclude these lines to toggle debouncing of mean/SD values
*   In version 0.16, I'm including the "debounce" code. The alternative is to not check the limits.
*   Fixed a bug that could cause the samples to be biased towards smaller numbers in the range, especially with more extreme SDs
    (thanks to Frank Gootjes for pointing this out).
*   Changed performance parameters to reduce the chance of missing a valid solution, especially with fixed values.
*   Added a progress counter to keep track of unique solutions as they are found.
*   Added a message to indicate when searching has finished and plotting of the results has started.
