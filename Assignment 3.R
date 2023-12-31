# Assignment 3 - Hangman

# Scan the text file to obtain the Hangman dictionary
library(readr)
file_path <- "~/Desktop/GitHub/Assignment-3/Dictionary.txt"

# Read the dictionary from the Dictionary.txt file
dictionary <- read_lines(file_path)

# Main Game Loop
while (TRUE) { # add an infinite loop to allow multiple games
  
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
  correct_letters <- rep("_", word_length) # this line allows the user to visualize the word in this format: " _ _ _ _ "
  wrong_letters <- character(0) # initializes the wrong_letters variable as an empty character vector so that later the incorrectly guessed letters can be added to it using the c() function
  wrong_attempts <- 0 # initializes the wrong_attempts variable beginning at 0 so that later the incorrect number of attempts will be added using "wrong attempts + 1" with a limit at 6 (max amt of attempts)
  incorrect_letters <- character(0)
  incorrect_words <- character(0)
  
  # Function to check if a character is a letter
  is_letter <- function(char) {
    grepl("^[A-Za-z]+$", char) # ^[A-Za-z]+$" ensures that if the user inputs a word that contains a string of letters and numbers it will display an error message
  }
  
  # Function to display the progress - within the main game loop display_progress will print the current state of the game with filled-in letters and underscores to provide feedback to the user
  display_progress <- function() {
    print(paste(correct_letters, collapse = " "))
  }
  
  # Store the previous guessed word
  previous_guessed_word <- ""
  
  # Loop for a single game
  while (wrong_attempts < max_attempts) { # this loop will run as long as the # of wrong attempts is less than 6 
    # Ask for user input
    user_input <- readline("Please enter a letter or type 'guess' to guess the entire word: ")
    # Convert the input to lowercase to allow the user to input either capital or lowercase letters; only lowercase letters will be displayed 
    user_input <- tolower(user_input)
    
    # Check if the input is a single letter or the entire word guess
    if (user_input == "guess") {
      guessed_word <- readline("Enter your guess for the whole word: ") 
      
      if (!is_letter(guessed_word)) {
        print("Please enter a word with letters only.")
        next
      }
      
      if (tolower(guessed_word) == tolower(answer)) { # if the guessed word is found in the answer
        correct_letters <- strsplit(answer, "")[[1]] # assign the correct letters to the answer
        print(paste("You did it Smarty Pants! You guessed '", answer, "' correctly!"))
        break
      } else { # if the guessed word is incorrect
        if (tolower(guessed_word) == tolower(previous_guessed_word)) {
          print("You have already guessed that word. Try again.")
          next
        }
        print("Wrong guess! Try again :)")
        wrong_attempts <- wrong_attempts + 1 # deduct an attempt for wrong word attempt; a wrong word attempt only counts as one attempt in this version of Hangman
        print(paste("Remaining tries:", max_attempts - wrong_attempts))
        previous_guessed_word <- guessed_word
        incorrect_words <- c(incorrect_words, guessed_word)
      }
    } else if (nchar(user_input) != 1 || !is_letter(user_input)) { # this checks if the user input is not a single letter
      print("Please enter a single letter.")
      next
    } else {
      # Check if the letter has already been guessed
      if (user_input %in% c(correct_letters, wrong_letters)) {
        print("You have already guessed that letter.")
        next # skip to the next part of the loop
      }
      # Check if the letter is in the answer
      if (grepl(user_input, answer, ignore.case = TRUE)) {
        print("Correct guess!") 
        indices <- strsplit(answer, "")[[1]] == user_input # find where the guessed letter is present in the answer
        correct_letters[indices] <- user_input # update the correct_letters vector with the correctly guessed letter
        display_progress() # display the progress with the updated correct letters and underscores; the correct letters will replace the underscores in the displayed output 
        
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
        wrong_letters <- c(wrong_letters, user_input) # update the incorrectly guessed letters to the wrong_letters vector
        incorrect_letters <- c(incorrect_letters, tolower(user_input))  # update the incorrectly guessed letter to the incorrect_letters vector
        wrong_attempts <- wrong_attempts + 1 # update the wrong_attempts vector
      }
    }
    
    print(paste("Wrong letters:", paste(unique(incorrect_letters), collapse = " "))) # print the list of wrong letters guessed
    print(paste("Incorrect words:", paste(incorrect_words, collapse = " "))) # print the list of wrong words guessed
    print(paste("Remaining attempts:", max_attempts - wrong_attempts)) # print the number of remaining attempts
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