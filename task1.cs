using System;
using System.IO;

namespace Task_1
{
    class Program
    {
        struct Frequency
        {
            public string Word { get; set; }
            public int Number { get; set; }

            public Frequency(string word, int number)
            {
                this.Word = word;
                this.Number = number;
            }
        };

        static void Main()
        {
            // Initial settings
            string filePath = "D:\\University\\Multi\\Lab 1\\Task 1\\text.txt";
            int wordsToShow = 25;
            bool includeAllWords = false;
            int minimumWordSize = 4;
            const int maximumWordsNumber = 10000;


            // Initial reading
            string text = File.ReadAllText(filePath);


            // Splitting text into words
            string[] words = new string[maximumWordsNumber];
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
                words[wordsIndex++] = buffer;
                buffer = "";
                i++;
                goto splitText;
            }

            int textSymbolAscii = (int)text[i];
            if (textSymbolAscii >= 65 && textSymbolAscii <= 90 || textSymbolAscii >= 97 && textSymbolAscii <= 122 || text[i] == '\'' || text[i] == '-')
            {
                buffer += text[i];
            }
            i++;
            goto splitText;


        // toLowerCase + removing useless words
        normalizeWords:
            int j = 0;
            string word;
            string[] normalizedWords = new string[maximumWordsNumber];
            int normalizedWordsIndex = 0;

        startNormalizeWord:
            if (j == wordsIndex)
            {
                goto termFrequency;
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


        // Count term frequency
        termFrequency:
            Frequency[] frequencies = new Frequency[maximumWordsNumber];
            int frequenciesIndex = 0;

            int l = 0;
        startCountingWords:
            if (l == normalizedWordsIndex)
            {
                goto mainSort;
            }

            string term = normalizedWords[l];

            bool hasAdded = false;
            int m = 0;
        startWordSearching:
            if (m == frequenciesIndex)
            {
                goto continueCountingWords;
            }

            if (frequencies[m].Word == term)
            {
                frequencies[m] = new Frequency(term, frequencies[m].Number + 1);
                hasAdded = true;
                goto continueCountingWords;
            }

            m++;
            goto startWordSearching;

        continueCountingWords:
            if (!hasAdded)
            {
                frequencies[frequenciesIndex++] = new Frequency(term, 1);
                hasAdded = true;
            }

            l++;
            goto startCountingWords;


        // Bubble sort of terms and their frequencies
        mainSort:
            int mainLoop = 0;
        startMainSort:
            if (mainLoop == frequenciesIndex)
            {
                goto printResults;
            }
       
            int innerLoop = mainLoop + 1;
        innerSort:
            if (innerLoop == frequenciesIndex)
            {
                goto continueMainSorting;
            }

            if (frequencies[mainLoop].Number >= frequencies[innerLoop].Number)
            {
                goto continueInnerSorting;
            }

            Frequency temp = frequencies[mainLoop];
            frequencies[mainLoop] = frequencies[innerLoop];
            frequencies[innerLoop] = temp;

        continueInnerSorting:
            innerLoop++;
            goto innerSort;

        continueMainSorting:
            mainLoop++;
            goto startMainSort;


        // Printing the result list
        printResults:
            Console.WriteLine(text);

            int h = 0;

        startPrintingResults:
            if (h == frequenciesIndex || h == wordsToShow - 1)
            {
                return;
            }

            Console.WriteLine($"{frequencies[h].Word} - {frequencies[h].Number}");

            h++;
            goto startPrintingResults;
        }
    }
}
