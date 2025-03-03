# An animation of the sieve of Eratosthenes using PyGame.
# This program has been created by DeepSeek with some minor
# changes made afterwards.

import pygame
import sys

# Constants
MAX_NUM   = 100 # Maximum number to sieve
GRID_COLS = 10  # Number of columns in the grid
CELL_SIZE = 50  # Size of each cell in pixels
MARGIN    = 5  # Margin between cells
WIDTH     = GRID_COLS * (CELL_SIZE + MARGIN) + MARGIN               # Screen width
HEIGHT    = (MAX_NUM // GRID_COLS) * (CELL_SIZE + MARGIN) + MARGIN  # Screen height

# Color definitions
COLORS = {
    0: (255, 255, 255),  # Unmarked cell        (white)
    1: (0, 0, 255),      # Current prime number (blue)
    2: (255, 0, 0),      # Marking multiple     (red)
    3: (128, 128, 128),  # Composite number     (gray)
    4: (0, 255, 0)       # Prime number         (green)
}

def sieve_animation(max_num):
    """Generator function implementing the Sieve of Eratosthenes with animation steps."""
    states = [0] * (max_num + 1)  # 0 to max_num
    states[0] = 3  # 0 is not a prime
    states[1] = 3  # 1 is not a prime
    yield states.copy()

    for current in range(2, max_num + 1):
        if states[current] == 3:
            continue  # Skip composite numbers
        if states[current] == 0:
            # Mark current number as the active prime
            states[current] = 1
            yield states.copy()

            # Mark all multiples of the current prime
            for multiple in range(current * current, max_num + 1, current):
                if states[multiple] in (0, 2, 3):
                    states[multiple] = 2  # Temporarily mark as active multiple
                    yield states.copy()
                    states[multiple] = 3  # Permanently mark as composite
                    yield states.copy()

            # Mark the current prime as confirmed
            states[current] = 4
            yield states.copy()

def main():
    """Main function to run the Pygame animation."""
    pygame.init()
    screen = pygame.display.set_mode((WIDTH, HEIGHT))
    pygame.display.set_caption("Sieve of Eratosthenes")
    font = pygame.font.Font(None, 24)  # Font for rendering numbers

    sieve_gen = sieve_animation(MAX_NUM)
    current_states = next(sieve_gen)  # Initialize the state

    # Set up timer to control animation speed (400ms per step)
    STEP_EVENT = pygame.USEREVENT + 1
    pygame.time.set_timer(STEP_EVENT, 400)

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            elif event.type == STEP_EVENT:
                try:
                    current_states = next(sieve_gen)
                except StopIteration:
                    pygame.time.set_timer(STEP_EVENT, 0)  # Stop when done

        # Clear the screen
        screen.fill((0, 0, 0))

        # Draw each number in the grid
        for num in range(1, MAX_NUM + 1):
            row = (num - 1) // GRID_COLS
            col = (num - 1) % GRID_COLS
            x = col * (CELL_SIZE + MARGIN) + MARGIN
            y = row * (CELL_SIZE + MARGIN) + MARGIN
            
            color = COLORS[current_states[num]]
            pygame.draw.rect(screen, color, (x, y, CELL_SIZE, CELL_SIZE))
            
            # Render the number text
            text = font.render(str(num), True, (0, 0, 0))
            text_rect = text.get_rect(center=(x + CELL_SIZE//2, y + CELL_SIZE//2))
            screen.blit(text, text_rect)

        pygame.display.flip()  # Update the display

    pygame.quit()
    sys.exit()

if __name__ == "__main__":
    main()
    
