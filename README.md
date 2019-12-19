# propel

Liam Koehlers GP fork of Nic Mcphees fork of Lee Spector's Plushy fork of Tom Helmuth's little PushGP implementation in Clojure.

## The problem:
The hope is to evolve a solution to converting a byte-string into a human readable format. This was inspired by [this blog post](https://programming.guide/worlds-most-copied-so-snippet.html), which talks about how the most-copied code snippet on Java StackOverflow is a (flawed) piece of code to do that conversion.

The original question can be [found here](https://stackoverflow.com/questions/3758606/how-to-convert-byte-size-into-human-readable-format-in-java/38390338)

In general, the goal is to make conversions of this form:
```
110592:       110.6 KB  
7077888:      7.1 MB    
452984832:    453.0 MB  
28991029248:  29.0 GB
```
### Simplifications:
I initially chose to simplify the problem in a number of ways.
 - **Use SI units:** Although I don't have any hard proof of this, my assumption is that dividing down by powers of 10 would be easier to handle then dividing down by powers of 1024. For this reason I chose to represent my data in [SI Units](https://www.wikiwand.com/en/Metric_prefix) such as MB, as opposed MiB.
 - **Reduce the number of postfixes:** Postfixes for bytes in SI go up to [Yottabytes](https://www.wikiwand.com/en/Yottabyte), which is 10^8. I made the assumption that locking the problem to a smaller set would help the genetic programming succeed. For this reason, I locked my test cases to: B, KB, MG, GB.

## Initial Setup:
My initial setup consisted of a fairly limited set of instructions. All integer operations were stripped out, as well as most string operations. Exec operations were left in. All SI posfixes were included as strings.

Errors for the strings were calculated by [Levenshtein distance](https://www.wikiwand.com/en/Levenshtein_distance).

Thirty-ish test cases were added, generated using a simple script I wrote. These can be seen in src/core.clj.

A new `double` stack was added.

The following unique operations were provided:
 - int_to_double: convert integer to double.
 - double_divide10: divide double by 10. I created this instruction because I thought that independantly figuring out how to divide by 10 was too much to ask.
 - double_trim: trimmed double to one decimal place, and converted to string. This was added to streamline the proccess of "outputting" a result.

The hope with this setup is that the algorothm would:
- Read in int
- Convert to double
- Divide double by 10 n times
- Convert to string
- Append the correct SI postfix

The success with this setup was abysmal. No meaningfull evolution occured. The algorithm would find a homogenous "solution" such as 12.0MB, and print this value, ignoring all inputs. This suggests there was some cliffs that the algorithm wanted to avoid.

## Improvements:
Over the course of the project, I fiddled with the problem a lot, and made many different attempts. I would itemize all of these attempts, but sadly they were all failures. Exceptions will be noted below.

### Strong error function:
One of the big things I added was a stronger error function. This was created by adding a three-double vector as the error, instead of a single number. This was build as:
- Levenshtein distance
- Levenstein distance of *only* the postfix (numbers stripped out)
- The absolute value of the numbers (postfix stripped out)

These three error vectors could be multiplied by a arbitrary ammount, which allowed me to "tune" what the algorithm was searching for. By multiply by 0, I could even "turn off" one of the error types.

### Re-adding better integer support:
Something that Nic pointed out is that I didn't give my algorithm a good way to "count" the number of divisions it was making. For this reason, I re-added all the integer functions, as well as 0-9 as primitives. I also added a `int_inc` functionality for incrementing the top of the integer stack.

### Adding exec_while
It was also pointed out that I didn't have any good looping controls. Exec while was added for this reason. Better boolean support was also added at this stage.

## Final Setup Results:
My final setup consisted as described above. You can see this final setup in src/core.clj.

Sadly, I wasn't able to make any meaningfull progress on the evolution. The algorithm would still print out a random homogenous value, such as 12.0MB. This would balance out after only 10-20 iterations, and would stay for upwards of a few hundred. Despite much effort in changing the population size, and tuning the error function, the algorithm was unable to learn.

### Limited success: Memorization
At one point, the algorithm did learn the numbers for the first 10 elements of the input, but sadly this was just memorization. It did not generalize at all to the harder (larger, less ordered) test cases.

## Why I think it failed:
I believe this algorithm failed because of the harshness of my error function. Since the input numbers are so large (such as 28991029248) and the output numbers are so small (such as 29.0), the penalty of the absolute value is very big. Levenshtein distance also penalizes this large discrepancy.

To avoid this huge error function, the algorithm needs to either: ignore input, and print a simple, median result such as 12.0MB, or it needs to use the double_div10 MANY times. The in-between "learning-stage" of using double_div10 a *few* times was penalized a lot. 

For this reason I believe the bot simply ignored the input numbers, and built its own reasoanable answers.
