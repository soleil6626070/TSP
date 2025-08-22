# Script written by GPT


#!/usr/bin/env python3
"""
TSP Simulated Annealing Animation Generator
Creates an animated visualization from tsp_log_best.txt file
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.gridspec import GridSpec
from PIL import Image
import os

def parse_log_file(filename='tsp_log_best.txt'):
    """Parse the TSP log file to extract iteration, length, temperature, and path data"""
    
    iterations = []
    lengths = []
    temperatures = []
    paths = []
    
    with open(filename, 'r') as f:
        # Skip header line
        next(f)
        
        for line in f:
            # Split by whitespace
            parts = line.strip().split()
            
            # Extract iteration, length, and temperature
            iteration = int(parts[0])
            length = float(parts[1])
            temperature = float(parts[2])
            
            # Extract path coordinates
            # The path is a comma-separated string of x,y pairs
            path_str = parts[3]
            # Split and filter out empty strings (from trailing comma)
            coords = [float(x) for x in path_str.split(',') if x]
            
            # Reshape into (n_cities+1, 2) array - includes return to start
            path = np.array(coords).reshape(-1, 2)
            
            iterations.append(iteration)
            lengths.append(length)
            temperatures.append(temperature)
            paths.append(path)
    
    return np.array(iterations), np.array(lengths), np.array(temperatures), paths

def create_animation():
    """Create the animated visualization"""
    
    print("Reading tsp_log_best.txt...")
    iterations, lengths, temperatures, paths = parse_log_file('tsp_log_best.txt')
    n_frames = len(iterations)
    
    print(f"Found {n_frames} data points")
    print(f"Iterations range: {iterations[0]} to {iterations[-1]}")
    print(f"Length range: {lengths.min():.2f} to {lengths.max():.2f}")
    print(f"Temperature range: {temperatures.min():.2e} to {temperatures.max():.2e}")
    
    # Setup the figure with subplots
    fig = plt.figure(figsize=(16, 6))
    gs = GridSpec(1, 3, figure=fig, width_ratios=[1.2, 1, 1])
    
    ax1 = fig.add_subplot(gs[0])  # Path visualization
    ax2 = fig.add_subplot(gs[1])  # Length over iterations
    ax3 = fig.add_subplot(gs[2])  # Temperature over iterations
    
    # Initialize plot elements
    path_line, = ax1.plot([], [], 'b-', linewidth=1.5, alpha=0.7)
    city_points, = ax1.plot([], [], 'ro', markersize=6)
    start_point, = ax1.plot([], [], 'g*', markersize=12)
    
    length_line, = ax2.plot([], [], 'b-', linewidth=2)
    current_length_point, = ax2.plot([], [], 'ro', markersize=8)
    
    temp_line, = ax3.semilogy([], [], 'r-', linewidth=2)
    current_temp_point, = ax3.semilogy([], [], 'ro', markersize=8)
    
    # Set up the axes
    ax1.set_xlim(-0.05, 1.05)
    ax1.set_ylim(-0.05, 1.05)
    ax1.set_xlabel('X')
    ax1.set_ylabel('Y')
    ax1.grid(True, alpha=0.3)
    
    ax2.set_xlim(iterations[0], iterations[-1])
    ax2.set_ylim(lengths.min() * 0.95, lengths.max() * 1.05)
    ax2.set_xlabel('Iteration')
    ax2.set_ylabel('Path Length')
    ax2.grid(True, alpha=0.3)
    
    ax3.set_xlim(iterations[0], iterations[-1])
    ax3.set_ylim(temperatures[temperatures > 0].min() * 0.5, temperatures.max() * 2)
    ax3.set_xlabel('Iteration')
    ax3.set_ylabel('Temperature (log scale)')
    ax3.grid(True, alpha=0.3)
    
    def init():
        """Initialize animation"""
        path_line.set_data([], [])
        city_points.set_data([], [])
        start_point.set_data([], [])
        length_line.set_data([], [])
        current_length_point.set_data([], [])
        temp_line.set_data([], [])
        current_temp_point.set_data([], [])
        return path_line, city_points, start_point, length_line, current_length_point, temp_line, current_temp_point
    
    def animate(frame):
        """Animation function"""
        
        # Update path visualization
        path = paths[frame]
        path_line.set_data(path[:, 0], path[:, 1])
        city_points.set_data(path[:-1, 0], path[:-1, 1])  # Don't plot the duplicate start city
        start_point.set_data([path[0, 0]], [path[0, 1]])
        
        # Update title with current iteration
        ax1.set_title(f'TSP Path - Iteration {iterations[frame]}')
        
        # Update length plot
        length_line.set_data(iterations[:frame+1], lengths[:frame+1])
        current_length_point.set_data([iterations[frame]], [lengths[frame]])
        ax2.set_title(f'Path Length: {lengths[frame]:.4f}')
        
        # Update temperature plot
        temp_line.set_data(iterations[:frame+1], temperatures[:frame+1])
        current_temp_point.set_data([iterations[frame]], [temperatures[frame]])
        ax3.set_title(f'Temperature: {temperatures[frame]:.2e}')
        
        return path_line, city_points, start_point, length_line, current_length_point, temp_line, current_temp_point
    
    # Create animation
    print("Creating animation...")
    anim = animation.FuncAnimation(
        fig, animate, init_func=init,
        frames=n_frames, interval=100,
        blit=True, repeat=True
    )
    
    # Save as GIF
    print("Saving as tsp_animation.gif...")
    writer = animation.PillowWriter(fps=10)
    anim.save('tsp_animation.gif', writer=writer)
    
    print("Animation saved as tsp_animation.gif")
    
    # Also save as MP4 if ffmpeg is available
    try:
        print("Attempting to save as tsp_animation.mp4...")
        writer = animation.FFMpegWriter(fps=10)
        anim.save('tsp_animation.mp4', writer=writer)
        print("Animation saved as tsp_animation.mp4")
    except:
        print("Could not save as MP4 (ffmpeg may not be installed)")
    
    plt.show()

def create_static_frames():
    """Alternative: Create individual PNG frames that can be assembled into GIF/video"""
    
    print("Reading tsp_log_best.txt...")
    iterations, lengths, temperatures, paths = parse_log_file('tsp_log_best.txt')
    n_frames = len(iterations)
    
    # Create output directory
    os.makedirs('frames_output', exist_ok=True)
    
    print(f"Creating {n_frames} frames...")
    
    for idx in range(n_frames):
        # Setup the figure with subplots
        fig = plt.figure(figsize=(16, 6))
        gs = GridSpec(1, 3, figure=fig, width_ratios=[1.2, 1, 1])
        
        ax1 = fig.add_subplot(gs[0])  # Path visualization
        ax2 = fig.add_subplot(gs[1])  # Length over iterations
        ax3 = fig.add_subplot(gs[2])  # Temperature over iterations
        
        # Plot 1: Path visualization
        path = paths[idx]
        ax1.plot(path[:, 0], path[:, 1], 'b-', linewidth=1.5, alpha=0.7)
        ax1.plot(path[:-1, 0], path[:-1, 1], 'ro', markersize=6)
        ax1.plot(path[0, 0], path[0, 1], 'g*', markersize=12, label='Start')
        ax1.set_xlim(-0.05, 1.05)
        ax1.set_ylim(-0.05, 1.05)
        ax1.set_xlabel('X')
        ax1.set_ylabel('Y')
        ax1.set_title(f'TSP Path - Iteration {iterations[idx]}')
        ax1.grid(True, alpha=0.3)
        ax1.legend()
        
        # Plot 2: Length evolution
        ax2.plot(iterations[:idx+1], lengths[:idx+1], 'b-', linewidth=2)
        ax2.plot(iterations[idx], lengths[idx], 'ro', markersize=8)
        ax2.set_xlim(iterations[0], iterations[-1])
        ax2.set_ylim(lengths.min() * 0.95, lengths.max() * 1.05)
        ax2.set_xlabel('Iteration')
        ax2.set_ylabel('Path Length')
        ax2.set_title(f'Path Length: {lengths[idx]:.4f}')
        ax2.grid(True, alpha=0.3)
        
        # Plot 3: Temperature evolution (log scale)
        ax3.semilogy(iterations[:idx+1], temperatures[:idx+1], 'r-', linewidth=2)
        ax3.semilogy(iterations[idx], temperatures[idx], 'ro', markersize=8)
        ax3.set_xlim(iterations[0], iterations[-1])
        ax3.set_ylim(temperatures[temperatures > 0].min() * 0.5, temperatures.max() * 2)
        ax3.set_xlabel('Iteration')
        ax3.set_ylabel('Temperature (log scale)')
        ax3.set_title(f'Temperature: {temperatures[idx]:.2e}')
        ax3.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        # Save frame
        frame_filename = f'frames_output/frame_{idx:04d}.png'
        plt.savefig(frame_filename, dpi=100, bbox_inches='tight')
        plt.close()
        
        if idx % 10 == 0:
            print(f"  Created frame {idx}/{n_frames}")
    
    print(f"All frames saved in frames_output/")
    
    # Create GIF from frames
    print("Creating GIF from frames...")
    frames = []
    for idx in range(n_frames):
        img = Image.open(f'frames_output/frame_{idx:04d}.png')
        frames.append(img)
    
    frames[0].save(
        'tsp_animation_from_frames.gif',
        save_all=True,
        append_images=frames[1:],
        duration=100,  # milliseconds per frame
        loop=0
    )
    print("Animation saved as tsp_animation_from_frames.gif")

if __name__ == "__main__":
    print("TSP Animation Generator")
    print("-" * 40)
    
    # Check if log file exists
    if not os.path.exists('tsp_log_best.txt'):
        print("Error: tsp_log_best.txt not found!")
        print("Make sure the Fortran program has been run and created the log file.")
        exit(1)
    
    # Method 1: Use matplotlib animation (recommended)
    create_animation()
    
    # Method 2: Create individual frames (optional, slower but more control)
    # Uncomment the line below if you want to also create individual PNG frames
    # create_static_frames()
    
    print("-" * 40)
    print("Done!")