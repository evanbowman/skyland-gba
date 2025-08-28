import numpy as np

def generate_megadrive_lut(size=32, saturation_boost=1.15, contrast_boost=1.1):
    """
    Generate a Sega Mega Drive/Genesis-inspired color LUT.

    The Mega Drive used 9-bit color (3 bits per channel: R3G3B3)
    This creates the characteristic quantized, slightly saturated look.

    :param size: Size of the LUT (32 for 5-bit GBA color space)
    :param saturation_boost: Factor to boost color saturation (Mega Drive characteristic)
    :param contrast_boost: Factor to boost contrast slightly
    :return: A LUT that approximates Mega Drive color characteristics
    """
    lut = np.zeros((size, size, size, 3), dtype=np.uint8)

    for r in range(size):
        for g in range(size):
            for b in range(size):
                # Normalize to [0, 1]
                norm_r = r / (size - 1)
                norm_g = g / (size - 1)
                norm_b = b / (size - 1)

                # Step 1: Apply Mega Drive quantization
                # Convert to 3-bit per channel (0-7 range), then back to normalized
                quantized_r = round(norm_r * 7) / 7
                quantized_g = round(norm_g * 7) / 7
                quantized_b = round(norm_b * 7) / 7

                # Step 2: Apply characteristic Mega Drive color adjustments
                # Convert to HSV-like space for saturation boost
                max_c = max(quantized_r, quantized_g, quantized_b)
                min_c = min(quantized_r, quantized_g, quantized_b)
                delta = max_c - min_c

                if max_c > 0:
                    saturation = delta / max_c
                    # Boost saturation, but not for very dark or very light colors
                    if 0.2 < max_c < 0.9 and saturation > 0.1:
                        # Calculate saturation boost
                        boosted_saturation = min(1.0, saturation * saturation_boost)

                        # Apply the saturation boost
                        if delta > 0:
                            # Reconstruct color with boosted saturation
                            quantized_r = max_c - (max_c - quantized_r) * (boosted_saturation / saturation)
                            quantized_g = max_c - (max_c - quantized_g) * (boosted_saturation / saturation)
                            quantized_b = max_c - (max_c - quantized_b) * (boosted_saturation / saturation)

                # Step 3: Apply slight contrast boost (Mega Drive CRT characteristic)
                # Use a subtle S-curve
                quantized_r = apply_contrast_curve(quantized_r, contrast_boost)
                quantized_g = apply_contrast_curve(quantized_g, contrast_boost)
                quantized_b = apply_contrast_curve(quantized_b, contrast_boost)

                # Step 4: Apply Mega Drive-specific color character
                # Slightly warm up the image (reduce blue in midtones)
                if 0.3 < max_c < 0.8:
                    quantized_b *= 0.95
                    quantized_r *= 1.02

                # Clamp and convert back to 5-bit range
                final_r = int(np.clip(quantized_r * 31, 0, 31))
                final_g = int(np.clip(quantized_g * 31, 0, 31))
                final_b = int(np.clip(quantized_b * 31, 0, 31))

                lut[r, g, b] = [final_r, final_g, final_b]

    return lut

def apply_contrast_curve(value, contrast_factor):
    """
    Apply a subtle S-curve for contrast enhancement.
    """
    # Center around 0.5, apply power curve, then shift back
    centered = value - 0.5
    if centered >= 0:
        curved = (centered * contrast_factor) ** (1.0 / contrast_factor)
    else:
        curved = -((abs(centered) * contrast_factor) ** (1.0 / contrast_factor))

    return np.clip(curved + 0.5, 0.0, 1.0)

def generate_accurate_megadrive_palette():
    """
    Generate the actual 512-color Mega Drive palette.
    This creates all possible combinations of 3-bit RGB values.
    """
    palette = []
    for r in range(8):  # 3-bit red (0-7)
        for g in range(8):  # 3-bit green (0-7)
            for b in range(8):  # 3-bit blue (0-7)
                # Convert 3-bit values to 8-bit (multiply by ~36.43 to get 0-255 range)
                r8 = int(r * 255 / 7)
                g8 = int(g * 255 / 7)
                b8 = int(b * 255 / 7)
                palette.append((r8, g8, b8))

    return palette

def map_to_nearest_megadrive_color(r, g, b):
    """
    Map an RGB color to the nearest Mega Drive palette color.
    """
    # Convert 5-bit input to 8-bit for comparison
    r8 = int(r * 255 / 31)
    g8 = int(g * 255 / 31)
    b8 = int(b * 255 / 31)

    # Find nearest 3-bit quantized values
    nearest_r3 = round(r * 7 / 31)
    nearest_g3 = round(g * 7 / 31)
    nearest_b3 = round(b * 7 / 31)

    # Convert back to 5-bit space
    result_r = int(nearest_r3 * 31 / 7)
    result_g = int(nearest_g3 * 31 / 7)
    result_b = int(nearest_b3 * 31 / 7)

    return np.clip([result_r, result_g, result_b], 0, 31)

def generate_strict_megadrive_lut(size=32):
    """
    Generate a strict Mega Drive LUT that maps colors to actual MD palette colors.
    """
    lut = np.zeros((size, size, size, 3), dtype=np.uint8)

    for r in range(size):
        for g in range(size):
            for b in range(size):
                lut[r, g, b] = map_to_nearest_megadrive_color(r, g, b)

    return lut

# Generate both versions
print("Generating Mega Drive color profile LUT...")

# Version 1: Characteristic Mega Drive look with enhancements
enhanced_lut = generate_megadrive_lut(saturation_boost=1.2, contrast_boost=1.1)
with open("megadrive_enhanced.dat", "wb") as f:
    f.write(enhanced_lut.tobytes())

# Version 2: Strict palette mapping
strict_lut = generate_strict_megadrive_lut()
with open("megadrive_strict.dat", "wb") as f:
    f.write(strict_lut.tobytes())

print("Generated two Mega Drive LUTs:")
print("- megadrive_enhanced.dat: Characteristic MD look with enhanced colors")
print("- megadrive_strict.dat: Strict mapping to actual MD 512-color palette")

# Optional: Save the actual Mega Drive palette for reference
megadrive_palette = generate_accurate_megadrive_palette()
print(f"Mega Drive palette contains {len(megadrive_palette)} colors")
