# Task 0

Prepare dev environment.

```
cabal sandbox init
cabal install --only-dependencies
```

# Task 1

Create a simple, single threaded web crawler.

## Input
<program> <count> <url>
program: path to the executable
count: number of sub-pages to follow
url: the starting url

## Output
Prints to stdout title of every page visited, and the length of the body text (w/o html tags) e.g.

```
"Very nice page" 1813
"Another page" 1111
```


# Task 2

Adapt the program from Task 1 to be multithreaded.


# Task 3

Create a trigram based sentence generator, where the trigrams are initialised from the body texts of
the crawled pages.

## Subtask 1

The original program for crawling will be amended to save the trigram state when
finishing.

## Input
<program> <count> <url> <trigram file>
program: path to the executable
count: number of sub-pages to follow
url: the starting url
trigram file: file to store generated trigrams

## Output
Prints to stdout title of every page visited, and the length of the body text (w/o html tags) e.g.

```
"Very nice page" 1813
"Another page" 1111
```

## Subtask 2

Create an executable that generates sentences based on the trigram file.


## Input
<program> <count> <trigram file>
program: path to the executable
count: number of sentences to generate
trigram file: file to retrieve the generated trigrams

## Output
Prints to stdout the generated sentences, separated by a new line e.g.

```
I was going home.
People like oranges and bananas.
```

