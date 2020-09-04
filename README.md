# cs463-smart-image-smashing-in-haskell

This project was previously implemented entirely in Java. For this project the biggest challenge was implementing the same
solution entirely in Haskell, which is a purely functional programming language, whose primitives do not include tools like
typical loops, which makes the solutions rely on programming with recursion.  The way to create and manipulate arrays is also
very recursive.k

This project was geared towards putting into practice Dynamic grids to create an application that takes an image in .png format
and taking all of the data about the image to turn it into a grid or rgb's that can be analyzed and transformed. The objective
is to make the image smaller and smaller in width (or in height) by finding the path of pixels from side to side (or top to bottom)
in the image that adds up to the smallest amount of Energy, and then removing that path. The effect of such process will make the 
picture be decreased in size while causing the least possible visual change as perceived by the human eye.

Task This implementation of the image transformation works this way:

Finds Energies. It inspects an image (represented as a 2D list of RGB triplets), and calculates a gradient for each pixel (roughly,
how sharp a change occurs in color values from one side to the other).

Finds a Cheapest Path. It uses those energy calculations to find the cheapest path vertically across the image, from any pixel on 
the top row to any pixel on the bottom row, but only getting to choose the next path-step from the three pixels that are below-left, 
directly-below, or below-right.

Removes the Path. It then removes those pixels on the cheapest path, creating a narrower image that tends to lose as little information
as possible. We can then write back out to a file and view our better image! (We can use the PPM format and the imagemagick tool to get
the job done).
