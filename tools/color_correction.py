# The code in this individual file is public domain.

import numpy as np

# NOTE: I wrote this based on Higan's color correction GLSL shader.
# I'm bad at math, so there certainly could be mistakes here...

def inverse_color_transform(r, g, b):
    # Reverse the final gamma correction
    r, g, b = r ** 2.2, g ** 2.2, b ** 2.2

    # Apply the inverse color mixing matrix
    r, g, b = (
        1.000 * r - 0.215 * g + 0.025 * b,
        -0.043 * r + 1.134 * g - 0.122 * b,
        -0.215 * r + 0.025 * g + 1.169 * b
    )

    # Reverse the initial gamma adjustment (gamma correction)
    r, g, b = r ** (1.0 / 4.0), g ** (1.0 / 4.0), b ** (1.0 / 4.0)

    # Scale to the 5-bit range (0-31 for GBA) and ensure it's clamped correctly
    return np.clip([r * 31, g * 31, b * 31], 0, 31).astype(np.uint8)

# Generate a sample LUT with this transformation
LUT_SIZE = 32  # GBA 5-bit color
lut = np.zeros((LUT_SIZE, LUT_SIZE, LUT_SIZE, 3), dtype=np.uint8)

for r in range(LUT_SIZE):
    for g in range(LUT_SIZE):
        for b in range(LUT_SIZE):
            # Normalize to [0, 1]
            rn, gn, bn = r / (LUT_SIZE - 1), g / (LUT_SIZE - 1), b / (LUT_SIZE - 1)
            lut[r, g, b] = inverse_color_transform(rn, gn, bn)


lut.tofile("agb_screen.dat")
print("LUT saved to 'agb_screen.dat'")
