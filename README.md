# postcodes
A single page application written in Elm for fetching info for UK postcodes.

The app works in elm-live using the --pushstate flag.

Tests can be run with elm-live from the root directory.

I have implemented the router, the simultaneous fetching of two sets of web data, validation and formatting. I have not implemented an autocomplete but I have added a history function that could be extended to become an autocomplete function.

I have validated the postcodes with the elm-tools/parser. This demonstrates the ability to give more specific error messages upon entereing invalid data.

I could not get the routing function to work on my AWS website in a short time, but it works on the local environment if you download it from here. I shall also bring in my laptop tomorrow just in case I need to demonstrate it on there.

