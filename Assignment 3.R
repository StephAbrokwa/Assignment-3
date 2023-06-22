# Assignment 3 - Hangman

# Scan the text file to obtain the Hangman dictionary
library(readr)
file_path <- "~/Desktop/GitHub/Assignment-3/Dictionary.txt"

# Read the dictionary from a txt file
dictionary <- read_lines(file_path)

# Choose a random word from the list 
answer <- sample(dictionary, 1)

# Inform the user about the length of the answer
word_length <- nchar(answer)
cat("Welcome to Exotic Fruit Hangman!\n")
cat("The answer has", word_length, "letters.\n")
cat("You have six attempts to solve this puzzle. All the best!\n")

# Set the maximum number of attempts allowed 
max_attempts <- 6

# Initialize variables
correct_letters <- rep("_", word_length)
wrong_letters <- character(0)
wrong_guesses <- 0

# Function to check if a character is a letter
is_letter <- function(char) {
  grepl("[A-Za-z]", char)
}

# Function to display the progress
display_progress <- function() {
  cat(paste(correct_letters, collapse = " "), "\n")
}

# Main game loop
while (wrong_guesses < max_attempts) {
  # Ask for user input
  user_input <- readline("Please enter a letter or type 'guess' to guess the entire word: ")
  # Convert the input to lowercase
  user_input <- tolower(user_input)
  
  # Check if the input is a single letter or the entire word guess
  if (user_input == "guess") {
    guessed_word <- readline("Enter your guess for the whole word: ")
    
    if (tolower(guessed_word) == tolower(answer)) {
      correct_letters <- strsplit(answer, "")[[1]]
      cat("You did it Smarty Pants! You guessed '", answer, "' correctly!\n")
      break
    } else {
      cat("Wrong guess! Try again :)\n")
      wrong_guesses <- wrong_guesses + 1  # Deduct an attempt for wrong word guess
      cat("Remaining tries:", max_attempts - wrong_guesses, "\n")
      next
    }
  } else if (nchar(user_input) != 1 || !is_letter(user_input)) {
    cat("Please enter a single letter.\n")
    next
  }
  
  # Check if the letter has already been guessed
  if (user_input %in% c(correct_letters, wrong_letters)) {
    cat("You have already guessed that letter.\n")
    next
  }
  
  # Check if the letter is in the answer
  if (grepl(user_input, answer, ignore.case = TRUE)) {
    cat("Correct guess!\n")
    indices <- strsplit(answer, "")[[1]] == user_input
    correct_letters[indices] <- user_input
    display_progress()
    
    # Check if the word is fully guessed
    if (all(correct_letters != "_")) {
      cat("You did it Smarty Pants! You guessed '", answer, "' correctly!\n")
      break
    }
  } else {
    cat("Another one bites the dust! Try again :)\n")
    wrong_letters <- c(wrong_letters, user_input)
    wrong_guesses <- wrong_guesses + 1
    cat("Wrong letters:", paste(wrong_letters, collapse = " "), "\n")
    cat("Remaining attempts:", max_attempts - wrong_guesses, "\n")
  }
}

# If all tries are exhausted, reveal the secret word
if (wrong_guesses == max_attempts) {
  cat("Uh oh silly goose! The answer was '", answer, "'. Better luck next time!\n")
}