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


save_lut_to_file(generate_soft_sepia_lut(), "sepia_lut.dat")
