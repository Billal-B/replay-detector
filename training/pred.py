from models import ResearchModels
import numpy as np
import os
import csv
import glob
from keras.models import load_model

from processor import process_image

def main():
    model = 'c3d'
    saved_model = None
    num_classes = 2
    seq_length = 20
    if model in ['conv_3d', 'c3d', 'lrcn']:
        data_type = 'images'
        image_shape = (80, 80, 3)
    elif model in ['lstm', 'mlp']:
        data_type = 'features'
        image_shape = None
    else:
        raise ValueError("Invalid model. See train.py for options.")

    sequence_folders = get_sequence_folders()
    names_and_sequences = get_all_sequences_in_memory(seq_length, image_shape, sequence_folders)
    print("Loading the model...")
    model = load_model('model/1567514047.2198603_replay_model.h5')
    print("Done loading the model.")
    logos = []
    not_logos = []
    for (name, sequence) in names_and_sequences:
        idx = name.split('_')[-1]
        pred = model.predict(sequence.reshape((1, seq_length, image_shape[0], image_shape[1], image_shape[2])))
        if pred[0][0] > 0.50:
            not_logos.append(idx)
        else:
            logos.append(idx)
    sorted_logos = sorted(logos, key=lambda item: (int(item.partition(' ')[0])
                                       if item[0].isdigit() else float('inf'), item))
    sorted_not_logos = sorted(not_logos, key=lambda item: (int(item.partition(' ')[0])
                                       if item[0].isdigit() else float('inf'), item))
    print("LOGOS : ")
    print(str(sorted_logos))
    print("***************************")
    print("NOT LOGOS : ")
    print(str(sorted_not_logos))


def get_all_sequences_in_memory(seq_length, image_shape, sequence_folders):
    sequences = []
    data = get_eval(sequence_folders)
    for row in data:
        frames = get_frames_for_sample(row)
        frames = rescale_list(frames, seq_length)
        sequence = build_image_sequence(frames, image_shape)
        sequences.append((row, np.array(sequence)))
    return sequences


def get_eval(folders):
    eval_data = []
    for item in folders:
        eval_data.append(item)
    return eval_data

def build_image_sequence(frames, image_shape):
        """Given a set of frames (filenames), build our sequence."""
        return [process_image(x, image_shape) for x in frames]

        
def get_frames_for_sample(sample):
    path = os.path.join('data', "eval", sample)
    images = sorted(glob.glob(os.path.join(path, '*png')))
    return images

def rescale_list(input_list, size):
        """Given a list and a size, return a rescaled/samples list. For example,
        if we want a list of size 5 and we have a list of size 25, return a new
        list of size five which is every 5th element of the origina list."""
        assert len(input_list) >= size

        # Get the number to skip between iterations.
        skip = len(input_list) // size

        # Build our new output.
        output = [input_list[i] for i in range(0, len(input_list), skip)]

        # Cut off the last one if needed.
        return output[:size]

def get_sequence_folders():
    data = os.listdir("data/eval")
    return data


if __name__ == '__main__':
    main()
