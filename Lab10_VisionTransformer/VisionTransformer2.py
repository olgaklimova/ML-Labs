# -*- coding: utf-8 -*-
"""параша.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1MiX_6znFImovyV00IVqhFtgZjcvQ92rz
"""

# загружаем набор данных со снеками

!pip install -q datasets

from datasets import load_dataset
dataset = load_dataset("Matthijs/snacks")
print(dataset)

# Создадим словарь, в котором сопоставим название снека с числом (для проведения классификации)
labels = dataset["train"].features["label"].names
num_labels = len(dataset["train"].features["label"].names)
label2id, id2label = dict(), dict()
for i, label in enumerate(labels):
    label2id[label] = i
    id2label[i] = label

print(label2id)
print(id2label)

import torch
import torch.nn as nn

# Создадим изображение для прогонки через модель, чтобы провереть что все размерности
toy_img = torch.rand(1, 3, 48, 48)

# Определим параметры для сверточной сети
num_channels = 3
hidden_size = 768
patch_size = 16

# Conv 2D - слой свертки
projection = nn.Conv2d(num_channels, hidden_size, kernel_size=patch_size,
             stride=patch_size)

# Подаем картинку в модель, чтобы посмотреть, что она определена корректно
out_projection = projection(toy_img)

print(f'Original image size: {toy_img.size()}')
print(f'Size after projection: {out_projection.size()}')

# Распремляем изображение после прогонки через сверточный слой

patch_embeddings = out_projection.flatten(2).transpose(1, 2)
print(f'Patch embedding size: {patch_embeddings.size()}')

# Определим токен [CLS], он необходим для корректной подачи данных в модель трансформера
batch_size = 1
cls_token = nn.Parameter(torch.randn(1, 1, hidden_size))
cls_tokens = cls_token.expand(batch_size, -1, -1)

# Добавим токен к вектору изображения
patch_embeddings = torch.cat((cls_tokens, patch_embeddings), dim=1)
print(f'Patch embedding size: {patch_embeddings.size()}')

# Определим эмбеддинги с информацией о позиции патча (кусочка картинки) в исходном изображении
position_embeddings = nn.Parameter(torch.randn(batch_size, 10, hidden_size))

# Добавим эмбеддинг к вектору изображения
input_embeddings = patch_embeddings + position_embeddings
print(f'Input embedding size: {input_embeddings.size()}')

# Определим параметры для модели ViT
num_heads = 12
num_layers = 12

# Реализуем один блок энкодера
transformer_encoder_layer = nn.TransformerEncoderLayer(
           d_model=hidden_size, nhead=num_heads,
           dim_feedforward=int(hidden_size * 4),
           dropout=0.1)

# Объединими несколько блоков для построения модели
transformer_encoder = nn.TransformerEncoder(
           encoder_layer=transformer_encoder_layer,
           num_layers=num_layers)

# Подадим тестову картинку в модель для проверки корректности
output_embeddings = transformer_encoder(input_embeddings)
print(f' Output embedding size: {output_embeddings.size()}')

# На данном этапе мы самостоятельно построили модель ViT, однако так как модель достаточно
# большая для ее обучения потребуется значительное количество вычислительных ресурсов и данных, поэтому
# для классификации рассматриваемого датасета со снеками возьмем предобученную подель, которую сможем дообучить на наш датасет

!pip install transformers

from transformers import ViTModel

# Загрузим модель по ссылке
model_checkpoint = 'google/vit-base-patch16-224-in21k'
model = ViTModel.from_pretrained(model_checkpoint, add_pooling_layer=False)

# Пример входного изображения для проверки корректности скачивания
input_img = torch.rand(batch_size, num_channels, 224, 224)

# Прогоним картинку через модель
output_embedding = model(input_img)
print(output_embedding)
print(f"Ouput embedding size: {output_embedding['last_hidden_state'].size()}")

num_labels = 20

# Определим классификатор, который будет состоять из одного линейного слоя (всего классифицируем на 20 классов)
classifier = nn.Linear(hidden_size, num_labels)

# Проверим, что задали слой верно
output_classification = classifier(output_embedding['last_hidden_state'][:, 0, :])
print(f"Output embedding size: {output_classification.size()}")

# Импортируем библиотеки для работы с массивами, тензорами, моделью ViT и изображениями

import numpy as np
import torch
import cv2
import torch.nn as nn
from transformers import ViTModel, ViTConfig
from torchvision import transforms
from torch.optim import Adam
from torch.utils.data import DataLoader
from tqdm import tqdm

# Создадим класс Dataset, с помощью которого проведем предобработку изображений (обрежем и нормализуем)
# А также для формирования данных типа Dataset для более удобной подачи в модель

class ImageDataset(torch.utils.data.Dataset):

  def __init__(self, input_data):

      self.input_data = input_data
      # Transform input data
      self.transform = transforms.Compose([
        transforms.ToTensor(),
        transforms.Resize((224, 224), antialias=True),
        transforms.Normalize(mean=[0.5, 0.5, 0.5],
                             std=[0.5, 0.5, 0.5])
        ])

  def __len__(self):
      return len(self.input_data)

  def get_images(self, idx):
      return self.transform(self.input_data[idx]['image'])

  def get_labels(self, idx):
      return self.input_data[idx]['label']

  def __getitem__(self, idx):
      # Get input data in a batch
      train_images = self.get_images(idx)
      train_labels = self.get_labels(idx)

      return train_images, train_labels

# Добавим к предобученной части модели ViT построенный нами линейный слой классификатора

class ViT(nn.Module):

  def __init__(self, config=ViTConfig(), num_labels=20,
               model_checkpoint='google/vit-base-patch16-224-in21k'):

        super(ViT, self).__init__()

        self.vit = ViTModel.from_pretrained(model_checkpoint, add_pooling_layer=False)
        self.classifier = (
            nn.Linear(config.hidden_size, num_labels)
        )

  def forward(self, x):

    x = self.vit(x)['last_hidden_state']
    output = self.classifier(x[:, 0, :])

    return output

# Функция обучения для модели

def model_train(dataset, epochs, learning_rate, bs):

    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda" if use_cuda else "cpu")

    # Загружаем модель на device (необходимо при испозьзовании GPU), определяем функцию потерь и оптимизатор
    model = ViT().to(device)
    criterion = nn.CrossEntropyLoss().to(device)
    optimizer = Adam(model.parameters(), lr=learning_rate)

    # Загружаем батч с изображениями
    train_dataset = ImageDataset(dataset)
    train_dataloader = DataLoader(train_dataset, num_workers=1, batch_size=bs, shuffle=True)

    # Цикл дообучения (дообучение, так как мы взяли предобученную модель)
    for i in range(epochs):
        total_acc_train = 0
        total_loss_train = 0.0

        for train_image, train_label in tqdm(train_dataloader):
            output = model(train_image.to(device))
            loss = criterion(output, train_label.to(device))
            acc = (output.argmax(dim=1) == train_label.to(device)).sum().item()
            total_acc_train += acc
            total_loss_train += loss.item()

            loss.backward()
            optimizer.step()
            optimizer.zero_grad()

        print(f'Epochs: {i + 1} | Loss: {total_loss_train / len(train_dataset): .3f} | Accuracy: {total_acc_train / len(train_dataset): .3f}')

    return model

# Гиппер параметры
EPOCHS = 10
LEARNING_RATE = 1e-4
BATCH_SIZE = 8

# Обученная модель
trained_model = model_train(dataset['train'], EPOCHS, LEARNING_RATE, BATCH_SIZE)

# Функция позволяющая получить класс картинки, который предсказала обученная сеть

def predict(img):

    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda" if use_cuda else "cpu")
    transform = transforms.Compose([
        transforms.ToTensor(),
        transforms.Resize((224, 224)),
        transforms.Normalize(mean=[0.5, 0.5, 0.5],
                             std=[0.5, 0.5, 0.5])
        ])

    img = transform(img)
    output = trained_model(img.unsqueeze(0).to(device))
    prediction = output.argmax(dim=1).item()

    return prediction

# Проверим точность классификации на тестовой выборке

sum = 0
for i in range(952):
  if predict(dataset['test'][i]['image']) == dataset['test'][i]['label']:
    sum += 1

print(f'Тестовая точность:{sum/952}')

print(5)