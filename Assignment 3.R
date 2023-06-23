# Assignment 3 - Hangman

# Scan the text file to obtain the Hangman dictionary
library(readr)
file_path <- "~/Desktop/GitHub/Assignment-3/Dictionary.txt"

# Read the dictionary from the Dictionary.txt file
dictionary <- read_lines(file_path)

# Main game loop
while (TRUE) { # Add an infinite loop to allow multiple games
  
  # Choose a random word from the list in the txt file
  answer <- sample(dictionary, 1)
  
  # Inform the user about the length of the answer and print the beginning messages
  word_length <- nchar(answer) 
  print("Welcome to Exotic Fruit Hangman!")
  print(paste("The answer has", word_length, "letters."))
  print("You have six attempts to solve this puzzle. All the best!")
  
  # Set the maximum number of attempts allowed; 6 attempts for each round
  max_attempts <- 6
  
  # Initialize the following variables
  correct_letters <- rep("_", word_length)
  wrong_letters <- character(0)
  wrong_attempts <- 0
  incorrect_letters <- character(0)
  incorrect_words <- character(0)
  
  # Function to check if a character is a letter
  is_letter <- function(char) {
    grepl("^[A-Za-z]+$", char)
  }
  
  # Function to display the progress
  display_progress <- function() {
    print(paste(correct_letters, collapse = " "))
  }
  
  # Store the previous guessed word
  previous_guessed_word <- ""
  
  # Loop for a single game
  while (wrong_attempts < max_attempts) {
    # Ask for user input
    user_input <- readline("Please enter a letter or type 'guess' to guess the entire word: ")
    user_input <- tolower(user_input)
    
    # Check if the input is a single letter or the entire word guess
    if (user_input == "guess") {
      guessed_word <- readline("Enter your guess for the whole word: ") 
      
      if (!is_letter(guessed_word)) {
        print("Please enter a word with letters only.")
        next
      }
      
      if (tolower(guessed_word) == tolower(answer)) {
        correct_letters <- strsplit(answer, "")[[1]]
        print(paste("You did it Smarty Pants! You guessed '", answer, "' correctly!"))
        break
      } else {
        if (tolower(guessed_word) == tolower(previous_guessed_word)) {
          print("You have already guessed that word. Try again.")
          next
        }
        print("Wrong guess! Try again :)")
        wrong_attempts <- wrong_attempts + 1
        print(paste("Remaining tries:", max_attempts - wrong_attempts))
        previous_guessed_word <- guessed_word
        incorrect_words <- c(incorrect_words, guessed_word)
      }
    } else if (nchar(user_input) != 1 || !is_letter(user_input)) {
      print("Please enter a single letter.")
      next
    } else {
      if (user_input %in% c(correct_letters, wrong_letters)) {
        print("You have already guessed that letter.")
        next
      }
      
      if (grepl(user_input, answer, ignore.case = TRUE)) {
        print("Correct guess!") 
        indices <- strsplit(answer, "")[[1]] == user_input
        correct_letters[indices] <- user_input
        display_progress()
        
        if (all(correct_letters != "_")) {
          print(paste("You did it Smarty Pants! You guessed '", answer, "' correctly!"))
          break
        }
      } else {
        if (tolower(user_input) %in% incorrect_letters) {
          print("You have already guessed that letter.")
          next
        }
        
        print("Another one bites the dust! Try again :)")
        wrong_letters <- c(wrong_letters, user_input)
        incorrect_letters <- c(incorrect_letters, tolower(user_input))
        wrong_attempts <- wrong_attempts + 1
      }
    }
    
    print(paste("Wrong letters:", paste(unique(incorrect_letters), collapse = " ")))
    print(paste("Incorrect words:", paste(incorrect_words, collapse = " ")))
    print(paste("Remaining attempts:", max_attempts - wrong_attempts))
  }
  
  if (wrong_attempts == max_attempts) {
    print(paste("Uh oh silly goose! The answer was '", answer, "'. Better luck next time!"))
  }
  
  play_again <- readline("Wanna take another stab at it? (yes/no): ") # if the user decides to use this loop they must type yes or no; it will automatically end if y, n, or anything else is inputted
  if (tolower(play_again) != "yes") {
    break
  }
  
  print("Let's play again!")
}