using System;
using System.IO;

namespace Task_2
{
    class Program
    {
        struct Index
        {
            public string Word;
            public int?[] Pages;
            public int Count;

            public Index(string word)
            {
                this.Word = word;
                this.Pages = new int?[1000];
                this.Count = 0;
            }
        }

        static void Main()
        {
            // Initial settings
            const string filePath = "D:\\University\\Multi\\Lab 1\\Task 2\\text.txt";
            const int minimumWordSize = 4;
            const bool includeAllWords = false;
            const int wordsPerPage = 245;
            const int maximumWordsSize = 1000000;
            const int pagesMaximum = 100;


            // Initial reading
            string text = File.ReadAllText(filePath);


            // Splitting text into words
            string[] words = new string[maximumWordsSize];
            int wordsIndex = 0;
            int i = 0;
            string buffer = "";
        splitText:
            if (i >= text.Length)
            {
                goto normalizeWords;
            }

            if (text[i] == ' ' || text[i] == '\n' || i == text.Length - 1)
            {
                if (buffer.Length > 0)
                {
                    words[wordsIndex++] = buffer;
                    buffer = "";
                    i++;
                    goto splitText;
                }
            }

            int textSymbolAscii = (int)text[i];
            if (textSymbolAscii >= 65 && textSymbolAscii <= 90 || textSymbolAscii >= 97 && textSymbolAscii <= 122)
            {
                buffer += text[i];
            }
            i++;
            goto splitText;


        // toLowerCase + removing useless words if needed
        normalizeWords:
            int j = 0;
            string word;
            string[] normalizedWords = new string[maximumWordsSize];
            int normalizedWordsIndex = 0;

        startNormalizeWord:
            if (j == wordsIndex)
            {
                goto indexWords;
            }

            word = words[j];

            int k = 0;
            char symbol;
        normalizeWord:
            if (k == word.Length)
            {
                goto continueNormalizingWords;
            }

            symbol = word[k];
            int wordSymbolAscii = (int)symbol;
            if (wordSymbolAscii >= 65 && wordSymbolAscii <= 90)
            {
                symbol = (char)(wordSymbolAscii + 32);
            }

            char[] symbolsArray = word.ToCharArray();
            symbolsArray[k] = symbol;
            word = new string(symbolsArray);
            k++;
            goto normalizeWord;

        continueNormalizingWords:
            if (includeAllWords || word.Length >= minimumWordSize)
            {
                normalizedWords[normalizedWordsIndex++] = word;
            }
            j++;
            goto startNormalizeWord;


        // Index words
        indexWords:
            Index[] indexes = new Index[maximumWordsSize];
            int indexesIndex = 0;
            int m = 0;

        startIndexingWords:
            if (normalizedWordsIndex == m)
            {
                goto mainSort;
            }

            int page = (m + 1) / wordsPerPage + 1;
            string term = normalizedWords[m];

            int l = 0;
            bool hasAdded = false;
        startWordSearching:
            if (l == indexesIndex)
            {
                goto continueIndexingWords;
            }

            if (indexes[l].Word == term)
            {
                int?[] pages = indexes[l].Pages;

                int o = 0;
                bool hasPage = false;
            startAddingPage:
                if (o == 1000 || pages[o] == null)
                {
                    goto continueAddingPage;
                }

                if (pages[o] == page)
                {
                    hasPage = true;
                    goto continueAddingPage;
                }

                o++;
                goto startAddingPage;

            continueAddingPage:
                if (!hasPage)
                {
                    pages[indexes[l].Count] = page;
                    indexes[l] = new Index(term)
                    {
                        Pages = pages,
                        Count = indexes[l].Count + 1
                    };
                }

                hasAdded = true;
                goto continueIndexingWords;
            }

            l++;
            goto startWordSearching;

        continueIndexingWords:
            if (!hasAdded)
            {
                int?[] pages = new int?[1000];
                pages[0] = page;
                indexes[indexesIndex++] = new Index(term)
                {
                    Pages = pages,
                    Count = 1
                };
                hasAdded = true;
            }

            m++;
            goto startIndexingWords;


        // Bubble sort of terms and their frequencies
        mainSort:
            int mainLoop = 0;
        startMainSort:
            if (mainLoop == indexesIndex)
            {
                goto printResults;
            }

            int innerLoop = mainLoop + 1;
        innerSort:
            if (innerLoop == indexesIndex)
            {
                goto continueMainSorting;
            }

            if (indexes[mainLoop].Word.CompareTo(indexes[innerLoop].Word) == -1)
            {
                goto continueInnerSorting;
            }

            Index temp = indexes[mainLoop];
            indexes[mainLoop] = indexes[innerLoop];
            indexes[innerLoop] = temp;

        continueInnerSorting:
            innerLoop++;
            goto innerSort;

        continueMainSorting:
            mainLoop++;
            goto startMainSort;


        // Printing the result list
        printResults:
            int h = 0;

        startPrintingResults:
            if (h == indexesIndex)
            {
                return;
            }

            string pagesString = "";
            int d = 0;
            int pagesNumber = 0;
            bool toIgnoreWord = false;
        joinPages:
            if(pagesNumber > pagesMaximum)
            {
                toIgnoreWord = true;
                goto continuePrintingResults;
            }
    
            if (d == indexes[h].Pages.Length || indexes[h].Pages[d] == null)
            {
                goto continuePrintingResults;
            }

            pagesNumber++;
            pagesString += (d != 0 ? ", ": "") + indexes[h].Pages[d];

            d++;
            goto joinPages;

        continuePrintingResults:
            if (!toIgnoreWord)
            {
                Console.WriteLine($"{indexes[h].Word} - {pagesString}");
            }
            else
            {
                toIgnoreWord = false;
            }
            h++;
            goto startPrintingResults;
        }
    }
}
