## Часть 1. Установка базовой системы Arch Linux

### 1. Настройка виртуальной машины в VirtualBox

Перед установкой создайте новую виртуальную машину:

- **Образ ISO**: скачайте установочный образ Arch Linux (amd64) с официального сайта и укажите его в настройках виртуальной машины в разделе «Носители» (Storage) для виртуального оптического привода.
- **EFI** (опционально): если хотите установку в режиме EFI, в настройках перейдите в **Система → Материнская плата** и включите **Enable EFI**. При первой загрузке после выбора ядра может потребоваться подождать 1–2 минуты.

### 2. Первоначальная загрузка с ISO

После загрузки с установочного ISO выполните шаги ниже.

#### Шаг 1: Проверка режима загрузки

```bash
ls /sys/firmware/efi/efivars
```

- Если видите список файлов — **UEFI**.
- Если ошибка «No such file or directory» — **BIOS (Legacy)**.

#### Шаг 2: Настройка клавиатуры (опционально)

```bash
loadkeys ru          # русская раскладка
loadkeys us          # обратно на английскую
```

#### Шаг 3: Проверка интернета

```bash
ping -c 3 archlinux.org
```

#### Шаг 4: Синхронизация времени

```bash
timedatectl set-ntp true
```

#### Шаг 5: Разметка диска

Посмотрите имя диска (обычно `/dev/sda`):

```bash
fdisk -l
```

Запустите `cfdisk`:

```bash
cfdisk /dev/sda
```

- Для **BIOS** выберите тип `dos` (MBR), создайте один раздел на весь диск и сделайте его **Bootable**.
- Для **UEFI** выберите тип `gpt`, создайте:
  - раздел 1: 512 MB, тип `EFI System`
  - раздел 2: оставшееся место, тип `Linux root (x86-64)`

Запишите изменения (Write) и выйдите (Quit).

#### Шаг 6: Форматирование разделов

**Для BIOS:**

```bash
mkfs.ext4 /dev/sda1
```

**Для UEFI:**

```bash
mkfs.fat -F32 /dev/sda1   # EFI
mkfs.ext4 /dev/sda2       # root
```

#### Шаг 7: Монтирование

**Для BIOS:**

```bash
mount /dev/sda1 /mnt
```

**Для UEFI:**

```bash
mount /dev/sda2 /mnt
mount --mkdir /dev/sda1 /mnt/boot
```

#### Шаг 8: Установка базовых пакетов

```bash
pacstrap -K /mnt base linux linux-firmware vim networkmanager virtualbox-guest-utils
```

#### Шаг 9: Генерация fstab

```bash
genfstab -U /mnt >> /mnt/etc/fstab
```

#### Шаг 10: Вход в установленную систему (chroot)

```bash
arch-chroot /mnt
```

#### Шаг 11: Базовая настройка системы

**Пароль root:**

```bash
passwd
```

**Часовой пояс**:

```bash
ln -sf /usr/share/zoneinfo/Europe/Moscow /etc/localtime
hwclock --systohc
```

**Локализация:**

```bash
vim /etc/locale.gen
# раскомментировать en_US.UTF-8 и ru_RU.UTF-8
locale-gen
echo "LANG=ru_RU.UTF-8" > /etc/locale.conf   # или en_US.UTF-8
```

**Имя компьютера:**

```bash
echo "arch-vbox" > /etc/hostname
```

**Сеть (NetworkManager):**

```bash
systemctl enable NetworkManager
```

#### Шаг 12: Загрузчик

**Для BIOS:**

```bash
pacman -S grub
grub-install --target=i386-pc /dev/sda
grub-mkconfig -o /boot/grub/grub.cfg
```

**Для UEFI:**

```bash
pacman -S grub efibootmgr
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=GRUB
grub-mkconfig -o /boot/grub/grub.cfg
```

#### Шаг 13: Активация гостевых служб VirtualBox

```bash
systemctl enable vboxservice
```

#### Шаг 14: Завершение установки

```bash
exit
umount -R /mnt
reboot
```

Не забудьте извлечь ISO из виртуального привода. Войдите как `root`.

---

## Часть 2. Установка и настройка Hyprland

### 1. Подготовка VirtualBox (выполняется на хосте)

При **выключенной** виртуальной машине:

Через графический интерфейс:

- **Настройки → Дисплей**
  - **Видеопамять**: 128 МБ или 256 МБ
  - **Графический контроллер**: VMSVGA
  - **Ускорение**: включить **3D-ускорение**

Или через командную строку (замените `"Arch-VM"` на имя вашей VM):

```bash
VBoxManage modifyvm "Arch-VM" --accelerate3d on
VBoxManage modifyvm "Arch-VM" --vram 256
```

### 2. Установка Hyprland и компонентов (в гостевой системе)

#### Установка пакетов

```bash
sudo pacman -S hyprland wayland kitty wl-clipboard
sudo pacman -S ttf-dejavu ttf-liberation noto-fonts ttf-firacode-nerd
sudo pacman -S polkit polkit-gnome
```

#### Создание обычного пользователя (если ещё не создан)

**Важно:** Hyprland нельзя запускать от root.

```bash
# Установка sudo
pacman -S sudo

# Настройка sudo
EDITOR=nano visudo
# раскомментировать строку: %wheel ALL=(ALL:ALL) ALL

# Создание пользователя
useradd -m -G wheel,storage,video,audio User   # замените на ваше имя
passwd User

# Добавление в группу vboxsf
sudo usermod -a -G vboxsf User
```

#### Базовый конфиг Hyprland

```bash
mkdir -p ~/.config/hypr
nano ~/.config/hypr/hyprland.conf
```

Содержимое:

```ini
# Монитор для VirtualBox
monitor=Virtual-1,preferred,auto,1
# Клавиши
# Запуск терминала Win + Q
bind = SUPER, Q, exec, kitty
# Выключение компьютера
bind = SUPER, M, exit
# Полный экран программы
bind = SUPER, M, fullscreen, 1

# Перемещение окон

# Перемещение фокуса между окнами (вариант со стрелками)
bind = SUPER, left, movefocus, l
bind = SUPER, right, movefocus, r
bind = SUPER, up, movefocus, u
bind = SUPER, down, movefocus, d

# Перемещение (свап) окон (вариант со стрелками)
bind = SUPER SHIFT, left, swapwindow, l
bind = SUPER SHIFT, right, swapwindow, r
bind = SUPER SHIFT, up, swapwindow, u
bind = SUPER SHIFT, down, swapwindow, d

# Переключение стороны деления (split)
bind = SUPER, J, togglesplit

# Команда запуска при старте системы
exec-once = /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
```

### 3. Настройка менеджера сессий (greetd)

Для корректной работы Wayland необходим менеджер сессий.

```bash
sudo pacman -S greetd
sudo mkdir -p /etc/greetd
sudo nano /etc/greetd/config.toml
```

Содержимое:

```toml
[terminal]
vt = 1

[default_session]
command = "WLR_RENDERER=pixman Hyprland"   # программный рендеринг для VirtualBox
user = "User"   # ваше имя пользователя
```

Включение и запуск:

```bash
sudo systemctl enable greetd --now
```

После этого система будет сразу запускать Hyprland. Если что-то пойдёт не так:

- Переключитесь на другую консоль: `Ctrl+Alt+F2` (или F3, F4)
- Войдите как root, исправьте конфиг и перезапустите: `systemctl restart greetd`

Доустановка пакетов в системе

```Bash
sudo pacman -S firefox fish fastfetch nginx mc 
```

### 4. Возможные проблемы и их решения

| Проблема | Решение |
|----------|---------|
| `XDG_RUNTIME_DIR is not set` | Использовать полноценный менеджер сессий (greetd, elogind) |
| `CBackend::create() failed!` | Установить `mesa`, включить 3D-ускорение, использовать `WLR_RENDERER=pixman` |
| Hyprland не запускается после перезагрузки | Нажмите `Ctrl+Alt+F2`, войдите root, проверьте `/etc/greetd/config.toml` и службу `greetd` |

---

### Дополнительные замечания

- **Соответствие версий**: версия `virtualbox-guest-utils` в гостевой системе должна совпадать с версией VirtualBox на хосте.
- **Общий буфер обмена**: включается в настройках VM на хосте: **Общие → Дополнительно → Общий буфер обмена**.
- **Мелкий текст в TTY**: измените масштаб экрана в VirtualBox (Display Scale-factor) до 2.00 или 3.00.
