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
print("Welcome to Exotic Fruit Hangman!")
print(paste("The answer has", word_length, "letters."))
print("You have six attempts to solve this puzzle. All the best!")

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
  print(paste(correct_letters, collapse = " "))
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
      print(paste("You did it Smarty Pants! You guessed '", answer, "' correctly!"))
      break
    } else {
      print("Wrong guess! Try again :)")
      wrong_guesses <- wrong_guesses + 1  # Deduct an attempt for wrong word guess
      print(paste("Remaining tries:", max_attempts - wrong_guesses))
      next
    }
  } else if (nchar(user_input) != 1 || !is_letter(user_input)) {
    print("Please enter a single letter.")
    next
  }
  
  # Check if the letter has already been guessed
  if (user_input %in% c(correct_letters, wrong_letters)) {
    print("You have already guessed that letter.")
    next
  }
  
  # Check if the letter is in the answer
  if (grepl(user_input, answer, ignore.case = TRUE)) {
    print("Correct guess!")
    indices <- strsplit(answer, "")[[1]] == user_input
    correct_letters[indices] <- user_input
    display_progress()
    
    # Check if the word is fully guessed
    if (all(correct_letters != "_")) {
      print(paste("You did it Smarty Pants! You guessed '", answer, "' correctly!"))
      break
    }
  } else {
    print("Another one bites the dust! Try again :)")
    wrong_letters <- c(wrong_letters, user_input)
    wrong_guesses <- wrong_guesses + 1
    print(paste("Wrong letters:", paste(wrong_letters, collapse = " ")))
    print(paste("Remaining attempts:", max_attempts - wrong_guesses))
  }
}

# If all tries are exhausted, reveal the secret word
if (wrong_guesses == max_attempts) {
  print(paste("Uh oh silly goose! The answer was '", answer, "'. Better luck next time!"))
}