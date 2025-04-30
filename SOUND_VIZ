# Load the Shiny library
library(shiny)

# 1. Define a simple mapping from letters to frequencies (Hz)
note_freq <- c(A = 262, B = 294, C = 330, D = 349, E = 392, F = 440, G = 494,
               H = 523, I = 587, J = 659, K = 698, L = 784, M = 880, N = 988,
               O = 1047, P = 1175, Q = 1319, R = 1397, S = 1568, T = 1760, U = 1976,
               V = 2093, W = 2349, X = 2489, Y = 2794, Z = 3136)

# --- Define Amplitudes ---
amplitude_loud <- 1.0  # Amplitude for uppercase letters
amplitude_soft <- 0.4  # Amplitude for lowercase letters

# --- Define the User Interface (UI) ---
ui <- fluidPage(
  titlePanel("Simple Sound Wave Visualizer (with Amplitude!)"),
  sidebarLayout(
    sidebarPanel(
      textInput("inputText", "Enter Text (A-Z, a-z):", value = "SoFt LOUD soFt"), # Example mixed case
      helpText("Use CAPS for LOUD sounds and lowercase for soft sounds.")
    ),
    mainPanel(
      plotOutput("soundPlot")
    )
  )
)

# --- Define the Server Logic ---
server <- function(input, output) {
  
  output$soundPlot <- renderPlot({
    # --- Get and Process Input ---
    user_text <- input$inputText
    req(user_text)
    
    # Split into characters *before* changing case
    original_chars <- strsplit(user_text, "")[[1]]
    
    # Create an uppercase version for frequency lookup
    chars_upper <- toupper(original_chars)
    
    # Look up frequencies based on the uppercase version
    frequencies <- note_freq[chars_upper]
    
    # Identify valid characters (A-Z, ignoring case for validity check)
    valid_indices <- !is.na(frequencies)
    
    # Get the valid frequencies
    valid_frequencies <- frequencies[valid_indices]
    # Get the *original case* valid characters
    valid_original_chars <- original_chars[valid_indices]
    
    req(length(valid_frequencies) > 0) # Ensure we have valid notes
    
    # --- Generate Wave Data ---
    duration_per_note <- 0.1
    sample_rate <- 2000
    t_note <- seq(0, duration_per_note, length.out = floor(duration_per_note * sample_rate))
    wave <- numeric(0)
    
    # Loop through each valid frequency and its original character
    for (i in 1:length(valid_frequencies)) {
      freq <- valid_frequencies[i]
      char <- valid_original_chars[i]
      
      # Determine amplitude based on original case
      current_amplitude <- ifelse(char %in% LETTERS, amplitude_loud, amplitude_soft)
      # Note: LETTERS is a built-in R constant for A-Z uppercase
      
      # Generate wave segment with the determined amplitude
      note_wave_segment <- current_amplitude * sin(2 * pi * freq * t_note)
      wave <- c(wave, note_wave_segment)
    }
    
    num_notes <- length(valid_frequencies)
    total_duration <- num_notes * duration_per_note
    t_total <- seq(0, total_duration, length.out = length(wave))
    
    # --- Create the Plot ---
    
    # Adjust plot margins slightly for labels
    par(mar = c(5.1, 4.1, 4.1, 2.1) + c(1.5, 0, 0, 0)) # Slightly more bottom margin
    
    # Create the main plot, include standard x-axis time labels now
    plot(t_total, wave,
         type = 'l',
         col = "dodgerblue",
         lwd = 1.5, # Slightly thicker line
         xlab = "", # Remove default label, add manually below
         ylab = "Amplitude",
         main = paste("Waveform for:", user_text) # Show original input in title
         # No xaxt="n" needed, we want the numeric axis
    )
    grid()
    
    # --- Add Custom X-axis Labels and Lines ---
    boundary_times <- seq(from = duration_per_note, by = duration_per_note, length.out = num_notes - 1)
    midpoint_times <- seq(from = duration_per_note / 2, by = duration_per_note, length.out = num_notes)
    
    if (num_notes > 1) {
      abline(v = boundary_times, col = "grey80", lty = "dashed") # Lighter grey
    }
    
    # Add the letter labels (using original case) below the numeric axis
    # Adjust 'line' parameter to position them correctly relative to numeric axis
    mtext(text = valid_original_chars, side = 1, line = 2.5, at = midpoint_times, cex = 0.9)
    
    # Add the overall X-axis title below the letters
    title(xlab = "Time (s) / Letter Segments", line = 4.0) # Adjust line
    
  }) # End renderPlot
}

# --- Run the Application ---
shinyApp(ui = ui, server = server)
