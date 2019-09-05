from models import ResearchModels

def main():
    model = 'c3d'
    saved_model = None#'data/checkpoints/c3d-images.001-0.805.hdf5'
    num_classes = 2
    seq_length = 19
    if model in ['conv_3d', 'c3d', 'lrcn']:
        data_type = 'images'
        image_shape = (80, 80, 3)
    elif model in ['lstm', 'mlp']:
        data_type = 'features'
        image_shape = None
    else:
        raise ValueError("Invalid model. See train.py for options.")

    X, y = data.get_all_sequences_in_memory('train', data_type)
    X_test, y_test = data.get_all_sequences_in_memory('test', data_type)
    rm = ResearchModels(num_classes, model, seq_length, saved_model)
    rm.model.predict()
