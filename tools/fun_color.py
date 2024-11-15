import numpy as np

def generate_soft_sepia_lut(blend_ratio=0.5):
    # Ensure blend_ratio is between 0 and 1
    blend_ratio = max(0.0, min(1.0, blend_ratio))

    lut = np.zeros((32, 32, 32, 3), dtype=np.uint8)

    for r in range(32):
        for g in range(32):
            for b in range(32):
                # Compute full sepia transformation
                sepia_r = min(31, int(0.393 * r + 0.769 * g + 0.189 * b))
                sepia_g = min(31, int(0.349 * r + 0.686 * g + 0.168 * b))
                sepia_b = min(31, int(0.272 * r + 0.534 * g + 0.131 * b))

                # Blend sepia and original color
                soft_r = int((1 - blend_ratio) * r + blend_ratio * sepia_r)
                soft_g = int((1 - blend_ratio) * g + blend_ratio * sepia_g)
                soft_b = int((1 - blend_ratio) * b + blend_ratio * sepia_b)

                # Store the blended color in the LUT
                lut[r, g, b] = [soft_r, soft_g, soft_b]

    return lut


def save_lut_to_file(lut, filename):
    lut.flatten().tofile(filename)


save_lut_to_file(generate_soft_sepia_lut(), "sepia.dat")



def generate_ukiyo_e_lut(size=32, blue_intensity=0.2, yellow_intensity=0.1, red_warmth=0.1,
                         saturation_threshold=0.3, teal_saturation=0.7, lightness_threshold=0.5):
    """
    Generate a ukiyo-e-inspired LUT with adjustments for only light, saturated blues,
    subdued yellows, and warm reds.

    :param size: Size of the LUT (default 32x32x32 for 5-bit color).
    :param blue_intensity: Intensity of the shift for saturated blue tones.
    :param yellow_intensity: Intensity of the shift for yellow tones.
    :param red_warmth: How much cool reds are pushed toward warmer tones.
    :param saturation_threshold: Minimum saturation level to apply blue adjustments.
    :param teal_saturation: Factor to reduce saturation of light blue-green tones.
    :param lightness_threshold: Minimum lightness level for applying teal desaturation.
    :return: A LUT based on ukiyo-e color principles.
    """
    lut = np.zeros((size, size, size, 3), dtype=np.uint8)

    for r in range(size):
        for g in range(size):
            for b in range(size):
                norm_r, norm_g, norm_b = r / (size - 1), g / (size - 1), b / (size - 1)

                # Convert to HSV to determine saturation and lightness
                max_c = max(norm_r, norm_g, norm_b)
                min_c = min(norm_r, norm_g, norm_b)
                delta = max_c - min_c
                saturation = delta / max_c if max_c > 0 else 0
                lightness = max_c  # Lightness as the maximum color channel value

                # Step 1: Shift only light, saturated blues away from violet hues
                if norm_b > norm_r and norm_b > norm_g and saturation >= saturation_threshold and lightness >= lightness_threshold:
                    if norm_r > 0.2:  # If red is high enough to make it violet
                        norm_r -= blue_intensity  # Shift red down to de-violet
                    adjusted_b = np.clip(norm_b - blue_intensity, 0, 1)

                    # Reduce the saturation of teal tones by blending with gray
                    adjusted_r = norm_r * teal_saturation + (1 - teal_saturation) * max_c
                    adjusted_g = norm_g * teal_saturation + (1 - teal_saturation) * max_c
                    adjusted_b = adjusted_b * teal_saturation + (1 - teal_saturation) * max_c
                else:
                    adjusted_r, adjusted_g, adjusted_b = norm_r, norm_g, norm_b

                if norm_g > norm_r and norm_g > norm_b and lightness > 0.4: # Light to medium greens
                    norm_r += 0.15 * norm_g  # Add red for olive tone
                    norm_b *= 0.5  # Desaturate to avoid intense green

                # Step 2: Adjust yellows for ocher range
                if norm_g > norm_r and norm_g > norm_b:  # Yellow dominant
                    adjusted_g = np.clip(adjusted_g - yellow_intensity, 0, 1)

                # Step 3: Adjust reds (warm up cool reds)
                if norm_r > norm_b and norm_r > norm_g:  # Red dominant
                    adjusted_r = np.clip(adjusted_r + red_warmth, 0, 1)

                # Store adjusted color values
                lut[r, g, b, 0] = int(adjusted_r * 31)  # Red channel (5-bit)
                lut[r, g, b, 1] = int(adjusted_g * 31)  # Green channel (5-bit)
                lut[r, g, b, 2] = int(adjusted_b * 31)  # Blue channel (5-bit)

    return lut

# Parameters
size = 32
blue_intensity = 0.2         # Moderate shift for saturated blues
yellow_intensity = 0.1       # Moderate control of yellows
red_warmth = 0.1             # Mild warm-up for reds
saturation_threshold = 0.3   # Minimum saturation to apply blue adjustment
teal_saturation = 0.7        # Saturation scaling factor for teal tones
lightness_threshold = 0.5    # Only desaturate blue-greens if light enough

# Generate and save the LUT
lut = generate_ukiyo_e_lut(size, blue_intensity, yellow_intensity, red_warmth, saturation_threshold, teal_saturation, lightness_threshold)

with open("ukiyo_e.dat", "wb") as f:
    f.write(lut.tobytes())
