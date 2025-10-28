import numpy as np

def rgb_to_hsv(r, g, b):
    """Convert RGB to HSV color space."""
    max_c = max(r, g, b)
    min_c = min(r, g, b)
    delta = max_c - min_c

    # Hue calculation
    if delta == 0:
        h = 0
    elif max_c == r:
        h = 60 * (((g - b) / delta) % 6)
    elif max_c == g:
        h = 60 * (((b - r) / delta) + 2)
    else:
        h = 60 * (((r - g) / delta) + 4)

    # Saturation calculation
    s = 0 if max_c == 0 else delta / max_c

    # Value
    v = max_c

    return h, s, v

def hsv_to_rgb(h, s, v):
    """Convert HSV back to RGB."""
    c = v * s
    x = c * (1 - abs((h / 60) % 2 - 1))
    m = v - c

    if 0 <= h < 60:
        r, g, b = c, x, 0
    elif 60 <= h < 120:
        r, g, b = x, c, 0
    elif 120 <= h < 180:
        r, g, b = 0, c, x
    elif 180 <= h < 240:
        r, g, b = 0, x, c
    elif 240 <= h < 300:
        r, g, b = x, 0, c
    else:
        r, g, b = c, 0, x

    return r + m, g + m, b + m

def generate_november_lut(size=32, warmth=0.7, blue_desaturation=0.6,
                          orange_shift=0.15, brown_depth=0.3):
    """
    Generate a November/Autumn color palette LUT inspired by Wes Anderson films.

    Characteristics:
    - Warm orange and yellow tones dominate
    - Blues become muted slate/grey-blue
    - Greens shift toward olive and brown
    - Overall warm, cozy autumn feeling

    :param size: LUT size (32 for 5-bit)
    :param warmth: How much to warm up the entire palette (0-1)
    :param blue_desaturation: How much to desaturate blues (0-1, higher = more grey)
    :param orange_shift: How much to shift colors toward orange
    :param brown_depth: How much to deepen browns and add richness
    """
    lut = np.zeros((size, size, size, 3), dtype=np.uint8)

    for r in range(size):
        for g in range(size):
            for b in range(size):
                # Normalize to [0, 1]
                norm_r = r / (size - 1)
                norm_g = g / (size - 1)
                norm_b = b / (size - 1)

                # Convert to HSV for easier manipulation
                h, s, v = rgb_to_hsv(norm_r, norm_g, norm_b)

                # Step 1: Handle blues - desaturate and shift to slate
                if 180 <= h < 270:  # Blue to purple range
                    # Desaturate significantly
                    s = s * (1 - blue_desaturation)

                    # Shift hue slightly toward cyan/slate
                    if h > 210:
                        h = h - 10

                    # Darken slightly for that slate blue look
                    v = v * 0.85

                    # Add a hint of warmth even to blues
                    result_r, result_g, result_b = hsv_to_rgb(h, s, v)
                    result_r = min(1.0, result_r + 0.05 * warmth)

                # Step 2: Handle greens - shift to olive/brown
                elif 80 <= h < 180:  # Green to cyan range
                    # Shift hue toward yellow/brown
                    h = h - 20

                    # Reduce saturation for olive tones
                    s = s * 0.7

                    # Add brown depth by reducing brightness and adding red
                    v = v * (1 - brown_depth * 0.3)
                    result_r, result_g, result_b = hsv_to_rgb(h, s, v)
                    result_r = min(1.0, result_r + brown_depth * 0.2)

                # Step 3: Handle reds/oranges/yellows - enhance and warm
                elif (h < 80) or (h >= 300):  # Red to yellow range
                    # Shift toward orange
                    if h < 30 or h >= 330:  # Deep reds
                        h = 20  # Shift toward orange-red
                    elif 30 <= h < 60:  # Orange-reds to orange
                        h = min(45, h + orange_shift * 100)  # Push toward bright orange

                    # Boost saturation for vibrant autumn colors
                    if v > 0.5:  # Bright colors
                        s = min(1.0, s * 1.2)
                        # Brightest oranges can go toward yellow-orange
                        if v > 0.7 and 30 <= h < 60:
                            h = min(50, h + 5)
                    else:  # Dark colors
                        # Deepen browns
                        s = s * 0.9
                        v = v * (1 - brown_depth * 0.2)

                    result_r, result_g, result_b = hsv_to_rgb(h, s, v)

                # Step 4: Handle purples/magentas - shift toward warm browns or muted tones
                else:  # Purple/magenta range (270-300)
                    # Desaturate and shift toward brown
                    s = s * 0.6
                    h = 30  # Shift toward brown
                    v = v * 0.8
                    result_r, result_g, result_b = hsv_to_rgb(h, s, v)

                # Step 5: Apply overall warmth
                result_r = min(1.0, result_r + warmth * 0.1)
                result_g = min(1.0, result_g + warmth * 0.05)
                result_b = max(0.0, result_b - warmth * 0.05)

                # Step 6: Apply subtle contrast adjustment (S-curve)
                result_r = apply_autumn_contrast(result_r)
                result_g = apply_autumn_contrast(result_g)
                result_b = apply_autumn_contrast(result_b)

                # Convert to 5-bit and store
                lut[r, g, b] = [
                    int(np.clip(result_r * 31, 0, 31)),
                    int(np.clip(result_g * 31, 0, 31)),
                    int(np.clip(result_b * 31, 0, 31))
                ]

    return lut

def apply_autumn_contrast(value):
    """Apply a subtle contrast curve that enhances autumn tones."""
    # Gentle S-curve
    if value < 0.5:
        return 0.5 * ((2 * value) ** 1.1)
    else:
        return 1 - 0.5 * ((2 * (1 - value)) ** 1.1)

def generate_late_autumn_lut(size=32):
    """
    Generate a darker, more muted late autumn palette.
    Think overcast November days with bare trees.
    """
    lut = np.zeros((size, size, size, 3), dtype=np.uint8)

    for r in range(size):
        for g in range(size):
            for b in range(size):
                norm_r = r / (size - 1)
                norm_g = g / (size - 1)
                norm_b = b / (size - 1)

                h, s, v = rgb_to_hsv(norm_r, norm_g, norm_b)

                # Overall desaturation and darkening
                s = s * 0.7
                v = v * 0.9

                # Shift everything toward grey-brown
                if 180 <= h < 270:  # Blues become grey-blue
                    s = s * 0.5
                elif 80 <= h < 180:  # Greens become grey-brown
                    h = 40  # Shift to brown
                    s = s * 0.6
                elif h < 80 or h >= 300:  # Warm colors become muted
                    h = min(40, max(20, h))  # Constrain to brown-orange range
                    s = s * 0.8

                result_r, result_g, result_b = hsv_to_rgb(h, s, v)

                # Add slight warm grey cast
                result_r = min(1.0, result_r + 0.05)
                result_g = min(1.0, result_g + 0.03)

                lut[r, g, b] = [
                    int(np.clip(result_r * 31, 0, 31)),
                    int(np.clip(result_g * 31, 0, 31)),
                    int(np.clip(result_b * 31, 0, 31))
                ]

    return lut

def generate_golden_hour_lut(size=32):
    """
    Generate a golden hour palette - warm, glowing, magical autumn light.
    """
    lut = np.zeros((size, size, size, 3), dtype=np.uint8)

    for r in range(size):
        for g in range(size):
            for b in range(size):
                norm_r = r / (size - 1)
                norm_g = g / (size - 1)
                norm_b = b / (size - 1)

                h, s, v = rgb_to_hsv(norm_r, norm_g, norm_b)

                # Everything gets a golden glow
                if 180 <= h < 270:  # Blues become warm grey
                    h = 40
                    s = s * 0.4
                    result_r, result_g, result_b = hsv_to_rgb(h, s, v)
                    result_r = min(1.0, result_r + 0.15)
                    result_g = min(1.0, result_g + 0.1)
                else:
                    # Push everything toward golden yellow-orange
                    if h > 60:
                        h = 60 - (h - 60) * 0.5

                    # Boost saturation in highlights
                    if v > 0.5:
                        s = min(1.0, s * 1.3)

                    result_r, result_g, result_b = hsv_to_rgb(h, s, v)

                    # Add golden glow to everything
                    result_r = min(1.0, result_r + 0.12)
                    result_g = min(1.0, result_g + 0.08)

                lut[r, g, b] = [
                    int(np.clip(result_r * 31, 0, 31)),
                    int(np.clip(result_g * 31, 0, 31)),
                    int(np.clip(result_b * 31, 0, 31))
                ]

    return lut

# Generate autumn-themed LUTs
print("Generating November/Autumn color profiles...")

# Main November palette
november_lut = generate_november_lut(
    warmth=0.7,
    blue_desaturation=0.65,
    orange_shift=0.18,
    brown_depth=0.35
)
with open("november.dat", "wb") as f:
    f.write(november_lut.tobytes())
print("Generated: november.dat - Main autumn palette with vibrant oranges and slate blues")

# # Late autumn variant
# late_autumn_lut = generate_late_autumn_lut()
# with open("late_autumn.dat", "wb") as f:
#     f.write(late_autumn_lut.tobytes())
# print("Generated: late_autumn.dat - Darker, muted late autumn tones")

# # Golden hour variant
# golden_hour_lut = generate_golden_hour_lut()
# with open("golden_hour.dat", "wb") as f:
#     f.write(golden_hour_lut.tobytes())
# print("Generated: golden_hour.dat - Warm, glowing golden light")
