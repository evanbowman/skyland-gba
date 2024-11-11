import struct
import os
import binascii

# This script should be ran from the directory where you stored the game save.
# This script extracts all files stored in Skyland.sav to /extracted_cart_ram.
# The game implements a log-structured filesystem in save memory.
# This script extracts the stored files.

try:
    import heatshrink2
    HEATSHRINK_AVAILABLE = True
    print("Heatshrink module is available for decompression.")
except ImportError:
    HEATSHRINK_AVAILABLE = False
    print("Warning: Heatshrink module is not installed. Skipping decompression.")
    print("You may run Heatshrink decompression from the command line with:")
    print("heatshrink2 decompress -w 8 -l 4 <in> <out>")

def extract_files_from_fs3_log(file_path, output_dir):
    with open(file_path, 'rb') as f:
        f.seek(8)
        magic = f.read(8)
        if magic != b'_FS3_LOG':
            raise ValueError("Invalid file format: Magic string not found")
        while True:
            record = read_record(f)
            if not record:
                break
            save_file(record, output_dir)

def read_record(f):
    try:
        # Step 1: Read the invalidate status (u16)
        invalidate_ = struct.unpack('<H', f.read(2))[0]

        # Step 2: Read the FileInfo
        # CRC (u8), Flags (2 bytes), Name Length (u8), Data Length (u16)
        crc_ = struct.unpack('<B', f.read(1))[0]
        flags_ = struct.unpack('<2B', f.read(2))
        name_length_ = struct.unpack('<B', f.read(1))[0]

        # If name_length_ is 0xFF, this indicates an invalid or non-existent record
        if name_length_ == 0xFF:
            print("Reached end of valid records (slack space).")
            return None  # Return None to stop further processing

        data_length_ = struct.unpack('<H', f.read(2))[0]

        # Step 3: Read the filename as raw bytes (to handle non-UTF-8 filenames)
        filename_raw = f.read(name_length_)

        # Attempt to decode the filename with UTF-8, fallback to a safe name if it fails
        try:
            filename = filename_raw.decode('utf-8').strip('\0')
        except UnicodeDecodeError:
            # If decoding fails, use a fallback name
            filename = f"file_{binascii.hexlify(filename_raw).decode()}"
            print(f"Warning: Filename contains invalid UTF-8. Using fallback name: {filename}")

        # Step 4: Handle padding after the filename (0 or 1 byte)
        padding_len = 1 if (name_length_ % 2) != 0 else 0
        f.read(padding_len)  # Skip padding byte if needed

        # Step 5: Read the file data
        file_data = f.read(data_length_)

        # Step 6: Handle padding after the file data (0 or 1 byte)
        padding_len = 1 if (data_length_ % 2) != 0 else 0
        f.read(padding_len)  # Skip padding byte if needed

        # Step 7: If the file is compressed, we will decompress it (only if heatshrink is available)
        if flags_[0] & 0x02:  # Check if the compressed flag is set
            if HEATSHRINK_AVAILABLE:
                print(f"File '{filename}' is compressed, decompressing...")
                file_data = decompress_heatshrink(file_data)
            else:
                print(f"Warning: Heatshrink module not found. Skipping decompression for file '{filename}'.")

        return {
            'filename': filename,
            'data': file_data,
            'crc': crc_,
            'invalidate': invalidate_,
        }

    except Exception as e:
        print(f"Error reading record: {e}")
        return None

def decompress_heatshrink(compressed_data):
    try:
        decompressed_data = heatshrink2.decompress(compressed_data,
                                                   window_sz2 = 8,
                                                   lookahead_sz2 = 4)
        return decompressed_data
    except Exception as e:
        print(f"Error during decompression: {e}")
        return compressed_data

def save_file(record, output_dir):
    filename = record['filename']
    full_path = filename.split('/')
    file_data = record['data']
    filename = os.path.basename(filename)
    file_path = os.path.join(output_dir, *full_path)
    os.makedirs(os.path.dirname(file_path), exist_ok=True)
    try:
        with open(file_path, 'wb') as output_file:
            output_file.write(file_data)
        print(f"Extracted file: {filename}")
    except Exception as e:
        print(f"Failed to write file '{filename}': {e}")

if __name__ == "__main__":
    input_file = 'Skyland.sav'
    output_dir = 'extracted_cart_ram'  # Make sure this folder is writable
    extract_files_from_fs3_log(input_file, output_dir)
