
## 1. Скачивание MSYS2

**Официальный сайт:** https://www.msys2.org/

Скачайте установщик:
- **msys2-x86_64-latest.exe** (примерно 100-150 МБ)

## 2. Установка MSYS2

### Процесс установки:
1. Запустите скачанный `.exe` файл
2. Укажите путь установки (рекомендуется `C:\msys64`)
3. Дождитесь завершения установки
4. **ВАЖНО:** После установки НЕ запускайте MSYS2 автоматически (снимите галочку)

### Первоначальная настройка:
1. Запустите **MSYS2 UCRT64** из меню Пуск
2. Выполните обновление пакетов (возможно, потребуется 2-3 раза):
   ```bash
   pacman -Syu
   ```
3. Если терминал закроется, снова запустите **MSYS2 UCRT64** и выполните:
   ```bash
   pacman -Su
   ```

## 3. Установка необходимых пакетов

В терминале **MSYS2 UCRT64** последовательно выполните:

### Базовые инструменты:
```bash
# Обновление базовых пакетов
pacman -S --needed base-devel

# Установка GCC и G++ (компиляторы)
pacman -S mingw-w64-ucrt-x86_64-gcc

# Установка GDB (отладчик)
pacman -S mingw-w64-ucrt-x86_64-gdb

# Установка Make
pacman -S mingw-w64-ucrt-x86_64-make

# Установка CMake (опционально)
pacman -S mingw-w64-ucrt-x86_64-cmake
```

### Дополнительные полезные пакеты:
```bash
# Git для контроля версий
pacman -S git

# Утилиты (grep, find, sed, awk и др.)
pacman -S msys2-grep msys2-findutils msys2-sed msys2-gawk

# Python (опционально)
pacman -S mingw-w64-ucrt-x86_64-python

# Nano редактор
pacman -S nano
```

## 4. Проверка установки

После установки проверьте, что все работает:

```bash
# Проверка версии GCC
gcc --version
# Ожидаемый вывод: gcc.exe (Rev13, Built by MSYS2 project) 15.2.0

# Проверка версии GDB
gdb --version
# Ожидаемый вывод: GNU gdb (GDB) 16.2

# Проверка путей
which gcc
# Ожидаемый вывод: /ucrt64/bin/gcc
```

## 5. Настройка VS Code

### Установите расширения в VS Code:

1. **C/C++** (Microsoft) — IntelliSense и отладка
2. **Code Runner** (Jun Han) — опционально, для быстрого запуска

### Создайте структуру папок проекта:

```
G:\Projects\CPP\          # Папка проекта
├── .vscode\              # Настройки VS Code
│   ├── settings.json     # Настройки проекта
│   ├── tasks.json        # Задачи сборки
│   ├── launch.json       # Отладка
│   └── c_cpp_properties.json  # IntelliSense
├── test.cpp              # Ваш код
└── build.bat             # Опционально: bat-скрипт для сборки
```

## 6. Файлы конфигурации VS Code

### Файл: `G:\Projects\CPP\.vscode\c_cpp_properties.json`

```json
{
    "configurations": [
        {
            "name": "UCRT64",
            "includePath": [
                "${workspaceFolder}/**",
                "C:/msys64/ucrt64/include",
                "C:/msys64/ucrt64/include/c++/15.2.0",
                "C:/msys64/ucrt64/include/c++/15.2.0/x86_64-w64-mingw32",
                "C:/msys64/ucrt64/lib/gcc/x86_64-w64-mingw32/15.2.0/include",
                "C:/msys64/ucrt64/lib/gcc/x86_64-w64-mingw32/15.2.0/include/c++",
                "C:/msys64/ucrt64/x86_64-w64-mingw32/include"
            ],
            "defines": [
                "_DEBUG",
                "UNICODE",
                "_UNICODE"
            ],
            "compilerPath": "C:/msys64/ucrt64/bin/g++.exe",
            "cStandard": "c17",
            "cppStandard": "c++17",
            "intelliSenseMode": "windows-gcc-x64"
        }
    ],
    "version": 4
}
```

### Файл: `G:\Projects\CPP\.vscode\tasks.json`

```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Build with UCRT64",
            "type": "shell",
            "command": "C:\\msys64\\ucrt64\\bin\\g++.exe",
            "args": [
                "-fdiagnostics-color=always",
                "-g",
                "${file}",
                "-o",
                "${workspaceFolder}\\${fileBasenameNoExtension}.exe",
                "-m64"
            ],
            "options": {
                "cwd": "${workspaceFolder}",
                "shell": {
                    "executable": "cmd.exe",
                    "args": ["/c"]
                }
            },
            "problemMatcher": {
                "base": "$gcc",
                "fileLocation": ["relative", "${workspaceFolder}"]
            },
            "group": {
                "kind": "build",
                "isDefault": true
            },
            "detail": "Compile with MSYS2 UCRT64 GCC"
        },
        {
            "label": "Run in cmd",
            "type": "shell",
            "command": "start",
            "args": [
                "cmd",
                "/k",
                "${workspaceFolder}\\${fileBasenameNoExtension}.exe"
            ],
            "options": {
                "shell": {
                    "executable": "cmd.exe",
                    "args": ["/c"]
                }
            },
            "dependsOn": ["Build with UCRT64"],
            "dependsOrder": "sequence",
            "problemMatcher": []
        },
        {
            "label": "Build and Run",
            "dependsOn": ["Build with UCRT64", "Run in cmd"],
            "dependsOrder": "sequence",
            "problemMatcher": []
        }
    ]
}
```

### Файл: `G:\Projects\CPP\.vscode\launch.json`

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Debug with UCRT64",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}\\${fileBasenameNoExtension}.exe",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [],
            "externalConsole": true,
            "MIMode": "gdb",
            "miDebuggerPath": "C:\\msys64\\ucrt64\\bin\\gdb.exe",
            "setupCommands": [
                {
                    "description": "Enable pretty printing for gdb",
                    "text": "-enable-pretty-printing",
                    "ignoreFailures": true
                }
            ],
            "preLaunchTask": "Build with UCRT64"
        },
        {
            "name": "Run without debugging",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}\\${fileBasenameNoExtension}.exe",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}",
            "environment": [],
            "externalConsole": true,
            "MIMode": "gdb"
        }
    ]
}
```

### Файл: `G:\Projects\CPP\.vscode\settings.json` (настройки проекта)

```json
{
    "C_Cpp.intelliSenseEngine": "default",
    "C_Cpp.default.compilerPath": "C:/msys64/ucrt64/bin/g++.exe",
    "C_Cpp.default.cppStandard": "c++17",
    "C_Cpp.default.cStandard": "c17",
    "C_Cpp.default.intelliSenseMode": "windows-gcc-x64",
    "terminal.integrated.env.windows": {
        "PATH": "C:/msys64/ucrt64/bin;C:/msys64/usr/bin;${env:PATH}"
    },
    "files.associations": {
        "*.cpp": "cpp",
        "*.h": "cpp"
    }
}
```

### Глобальные настройки VS Code (`settings.json`):

Нажмите `Ctrl+Shift+P` → "Preferences: Open Settings (JSON)" и добавьте:

```json
{
    "workbench.startupEditor": "none",
    "editor.fontSize": 14,
    "C_Cpp.intelliSenseEngine": "default",
    "terminal.integrated.profiles.windows": {
        "UCRT64": {
            "path": "C:/msys64/msys2_shell.cmd",
            "args": ["-ucrt64", "-defterm", "-shell", "bash", "-l"],
            "icon": "terminal-bash"
        }
    },
    "terminal.integrated.defaultProfile.windows": "PowerShell"
}
```

## 7. Тестовый файл

### Файл: `G:\Projects\CPP\test.cpp`

```cpp
#include <iostream>
#include <vector>
#include <string>

int main() {
    std::cout << "=== MSYS2 UCRT64 with GCC ===\n" << std::endl;
    
    std::vector<std::string> messages = {
        "Hello from C++!",
        "VS Code is configured correctly",
        "IntelliSense is working"
    };
    
    for (const auto& msg : messages) {
        std::cout << "✓ " << msg << std::endl;
    }
    
    std::cout << "\nPress Enter to exit...";
    std::cin.get();
    
    return 0;
}
```

## 8. Использование

| Действие | Команда |
|----------|---------|
| **Сборка** | `Ctrl+Shift+B` → "Build with UCRT64" |
| **Сборка и запуск** | `Ctrl+Shift+B` → "Build and Run" |
| **Запуск (уже собранного)** | `Ctrl+Shift+P` → "Tasks: Run Task" → "Run in cmd" |
| **Отладка** | `F5` (выбрать "Debug with UCRT64") |
| **Запуск без отладки** | `Ctrl+F5` |


## 9. Полезные команды MSYS2

```bash
# Обновление всех пакетов
pacman -Syu

# Поиск пакета
pacman -Ss <имя_пакета>

# Установка пакета
pacman -S <имя_пакета>

# Удаление пакета
pacman -R <имя_пакета>

# Список установленных пакетов
pacman -Q

# Информация о пакете
pacman -Qi <имя_пакета>
```

---

## Готово!

Теперь у вас полностью настроенная среда для разработки на C++ с MSYS2 и VS Code!
