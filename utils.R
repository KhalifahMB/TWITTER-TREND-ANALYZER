# utils.R - Helper functions for the Twitter Analyzer

#' Error handling wrapper
safe_operation <- function(expr, error_value = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      message("Error: ", conditionMessage(e))
      return(error_value)
    },
    warning = function(w) {
      message("Warning: ", conditionMessage(w))
      return(error_value)
    }
  )
}

#' Enhanced emoji mapping
create_emoji_mapping <- function() {
  list(
    # Positive emojis
    "😀" = "grinning_face", "😃" = "grinning_face_with_big_eyes",
    "😄" = "grinning_face_with_smiling_eyes", "😁" = "beaming_face_with_smiling_eyes",
    "😆" = "grinning_squinting_face", "😅" = "grinning_face_with_sweat",
    "🤣" = "rolling_on_the_floor_laughing", "😂" = "face_with_tears_of_joy",
    "🙂" = "slightly_smiling_face", "🙃" = "upside_down_face",
    "😉" = "winking_face", "😊" = "smiling_face_with_smiling_eyes",
    "😇" = "smiling_face_with_halo", "🥰" = "smiling_face_with_hearts",
    "😍" = "smiling_face_with_heart_eyes", "🤩" = "star_struck",
    "😘" = "face_blowing_a_kiss", "😗" = "kissing_face",
    "😚" = "kissing_face_with_closed_eyes", "😙" = "kissing_face_with_smiling_eyes",
    "🥲" = "smiling_face_with_tear", "😋" = "face_savoring_food",
    "😛" = "face_with_tongue", "😜" = "winking_face_with_tongue",
    "🤪" = "zany_face", "😝" = "squinting_face_with_tongue",
    "🤑" = "money_mouth_face", "🤗" = "hugging_face",
    "🤭" = "face_with_hand_over_mouth", "🤫" = "shushing_face",
    "🤔" = "thinking_face", "🤨" = "face_with_raised_eyebrow",
    "😐" = "neutral_face", "😑" = "expressionless_face",
    "😶" = "face_without_mouth", "😏" = "smirking_face",
    "😒" = "unamused_face", "🙄" = "face_with_rolling_eyes",
    "😬" = "grimacing_face", "🤥" = "lying_face",
    # Negative emojis
    "😌" = "relieved_face", "😔" = "pensive_face",
    "😪" = "sleepy_face", "🤤" = "drooling_face",
    "😴" = "sleeping_face", "😷" = "face_with_medical_mask",
    "🤒" = "face_with_thermometer", "🤕" = "face_with_head_bandage",
    "🤢" = "nauseated_face", "🤮" = "face_vomiting",
    "🤧" = "sneezing_face", "🥵" = "hot_face",
    "🥶" = "cold_face", "🥴" = "woozy_face",
    "😵" = "dizzy_face", "🤯" = "exploding_head",
    "🤠" = "cowboy_hat_face", "🥳" = "partying_face",
    "😎" = "smiling_face_with_sunglasses", "🤓" = "nerd_face",
    "🧐" = "face_with_monocle", "😕" = "confused_face",
    "😟" = "worried_face", "🙁" = "slightly_frowning_face",
    "☹️" = "frowning_face", "😮" = "face_with_open_mouth",
    "😯" = "hushed_face", "😲" = "astonished_face",
    "😳" = "flushed_face", "🥺" = "pleading_face",
    "😦" = "frowning_face_with_open_mouth", "😧" = "anguished_face",
    "😨" = "fearful_face", "😰" = "anxious_face_with_sweat",
    "😥" = "sad_but_relieved_face", "😢" = "crying_face",
    "😭" = "loudly_crying_face", "😱" = "face_screaming_in_fear",
    "😖" = "confounded_face", "😣" = "persevering_face",
    "😞" = "disappointed_face", "😓" = "downcast_face_with_sweat",
    "😩" = "weary_face", "😫" = "tired_face",
    "🥱" = "yawning_face", "😤" = "face_with_steam_from_nose",
    "😡" = "pouting_face", "😠" = "angry_face",
    "🤬" = "face_with_symbols_on_mouth", "😈" = "smiling_face_with_horns",
    "👿" = "angry_face_with_horns", "💀" = "skull",
    "☠️" = "skull_and_crossbones", "💩" = "pile_of_poo",
    "🤡" = "clown_face", "👹" = "ogre",
    "👺" = "goblin", "👻" = "ghost",
    "👽" = "alien", "👾" = "alien_monster",
    "🤖" = "robot_face", "😺" = "grinning_cat",
    "😸" = "grinning_cat_with_smiling_eyes", "😹" = "cat_with_tears_of_joy",
    "😻" = "smiling_cat_with_heart_eyes", "😼" = "cat_with_wry_smile",
    "😽" = "kissing_cat", "🙀" = "weary_cat",
    "😿" = "crying_cat", "😾" = "pouting_cat"
  )
}

#' Enhanced tweet cleaning function
clean_tweet <- function(text) {
  if (is.null(text) || is.na(text) || !is.character(text)) {
    return("")
  }

  # Force conversion to UTF-8 and replace invalid sequences
  text <- stringi::stri_enc_toutf8(text, validate = TRUE)
  text <- iconv(text, "UTF-8", "UTF-8", sub = " ")

  # Map emojis to words
  emoji_map <- create_emoji_mapping()
  for (emoji in names(emoji_map)) {
    text <- stringr::str_replace_all(text, stringr::fixed(emoji), paste0(" ", emoji_map[[emoji]], " "))
  }

  # Standard cleaning
  text <- tolower(text)
  text <- stringr::str_replace_all(text, "https?://\\S+|www\\.\\S+", " ") # Remove URLs
  text <- stringr::str_replace_all(text, "@\\w+", " ") # Remove mentions
  text <- stringr::str_replace_all(text, "#(\\w+)", "\\1") # Remove # but keep the word
  text <- stringr::str_replace_all(text, "[^[:alnum:]\\s]", " ") # Remove punctuation
  text <- stringr::str_replace_all(text, "\\s+", " ") # Collapse whitespace
  text <- stringr::str_trim(text) # Trim whitespace

  return(text)
}

#' Validate hashtag input
validate_hashtag <- function(hashtag) {
  if (is.null(hashtag) || is.na(hashtag) || hashtag == "") {
    return(FALSE)
  }

  # Remove # if present
  hashtag <- gsub("^#", "", hashtag)

  # Check for valid characters (letters, numbers, underscore)
  if (!grepl("^[a-zA-Z0-9_]+$", hashtag)) {
    return(FALSE)
  }

  # Check length (Twitter max is 50 characters)
  if (nchar(hashtag) > 50) {
    return(FALSE)
  }

  return(TRUE)
}

#' Load tweets from CSV with error handling
load_tweets_csv <- function(file_path) {
  safe_read <- safe_operation(
    {
      df <- readr::read_csv(
        file_path,
        col_types = readr::cols(),
        locale = readr::locale(encoding = "UTF-8")
      )
      df <- df[!is.na(df$text) & df$text != "", ]
      if (nrow(df) == 0) {
        stop("No valid tweets found in the CSV file")
      }
      df
    },
    error_value = NULL
  )

  return(safe_read)
}
