# Load the Shiny library
library(shiny)

# --- Define Helper Data/Functions ---

# Simplified Note Frequency Mapping (Using notes from A4 upwards for A-G)
# A = 440 Hz (A4)
# B = 493.88 Hz (B4)
# C = 523.25 Hz (C5)
# D = 587.33 Hz (D5)
# E = 659.25 Hz (E5)
# F = 698.46 Hz (F5)
# G = 783.99 Hz (G5)
note_freqs <- c(
  A = 440.00, B = 493.88, C = 523.25, D = 587.33,
  E = 659.25, F = 698.46, G = 783.99
)
# We will only process letters A-G in this simple version.

# --- UI Definition ---
# Defines the user interface layout
ui <- fluidPage(
  titlePanel("Simple Sound Wave Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      # Text input for the user
      textInput("inputText",
                "Enter a short word (uses A-G):",
                value = "CAB"), # Default value so something appears initially
      
      # Explanation for the user
      helpText("Enter letters (A-G). Other characters are ignored.",
               "Each recognized letter generates a short sine wave sequentially based on its pitch.")
    ),
    
    mainPanel(
      # Output: Plot the generated waveform
      plotOutput("waveformPlot")
    )
  )
)

# --- Server Logic ---
# Defines how inputs are processed and outputs are generated
server <- function(input, output) {
  
  # Reactive expression to generate the plot
  output$waveformPlot <- renderPlot({
    
    # 1. Get and Validate Input Text
    text_in <- input$inputText
    # Handle empty or null input gracefully
    if (is.null(text_in) || nchar(trimws(text_in)) == 0) {
      plot(NULL, xlim = c(0, 1), ylim = c(-1, 1), xlab = "Time (s)", ylab = "Amplitude", main = "Please enter some text (A-G)")
      return() # Stop if no input
    }
    
    # 2. Preprocess Input Text
    text_upper <- toupper(text_in) # Convert to uppercase
    chars <- strsplit(text_upper, "")[[1]] # Split into individual characters
    
    # Filter out characters that are not in our frequency map (A-G)
    valid_chars <- chars[chars %in% names(note_freqs)]
    
    # Handle case where no valid characters are found
    if (length(valid_chars) == 0) {
      plot(NULL, xlim = c(0, 1), ylim = c(-1, 1), xlab = "Time (s)", ylab = "Amplitude", main = paste("No valid letters (A-G) found in:", text_in))
      return() # Stop if no valid characters
    }
    
    # 3. Define Wave Parameters
    sample_rate <- 8000  # Hz (Keep it relatively low for faster plotting in demo)
    duration_per_note <- 0.2 # seconds (How long each note's wave lasts)
    amplitude <- 0.8 # Keep amplitude slightly below 1
    
    # 4. Generate Waveform Sequentially
    total_wave <- numeric(0) # Initialize an empty vector to store the full wave
    
    for (char in valid_chars) {
      freq <- note_freqs[char] # Get frequency for the character
      
      # Generate time vector for this specific note segment
      # Ensure t goes up to duration_per_note but doesn't exceed it much
      num_samples_per_note <- floor(duration_per_note * sample_rate)
      t_note <- seq(0, by = 1/sample_rate, length.out = num_samples_per_note)
      
      # Generate sine wave segment for this note
      # Formula: Amplitude * sin(2 * pi * frequency * time)
      wave_segment <- amplitude * sin(2 * pi * freq * t_note)
      
      # Append this segment to the total wave
      total_wave <- c(total_wave, wave_segment)
    }
    
    # 5. Create Time Axis for Plotting
    # The time axis should correspond to the length and sample rate of the total_wave
    time_axis <- seq(from = 0, by = 1/sample_rate, length.out = length(total_wave))
    
    # 6. Plot the Combined Waveform
    plot(time_axis, total_wave, type = 'l', # 'l' for line plot
         xlab = "Time (s)",
         ylab = "Amplitude",
         main = paste("Combined Waveform for:", paste(valid_chars, collapse="")),
         ylim = c(-1, 1), # Set Y-axis limits for amplitude
         col = "steelblue", # Set line color
         lwd = 1) # Set line width
    abline(h = 0, col = "grey", lty = 2) # Add a horizontal line at zero amplitude
    
  }) # End renderPlot
  
} # End server

# --- Run the Application ---
shinyApp(ui = ui, server = server)
