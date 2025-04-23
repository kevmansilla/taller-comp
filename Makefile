# Nombre del archivo de salida
EXEC = test_lab

# Archivos fuente
SRC = main.hs

# Comando para compilar con GHC
GHC = ghc

# Reglas para compilar y ejecutar los tests
all: $(EXEC)

$(EXEC): $(SRC)
	@echo "Compilando tests..."
	$(GHC) -o $(EXEC) $(SRC)
	@if [ $$? -ne 0 ]; then \
		echo "❌ Error de compilación en los tests"; \
		exit 1; \
	fi
	@echo "Compilación exitosa"

# Regla para ejecutar los tests
run: $(EXEC)
	@echo "Ejecutando tests..."
	./$(EXEC)

# Limpiar archivos generados
clean:
	@echo "Limpiando archivos generados..."
	rm -f $(EXEC) *.hi *.o

# Regla por defecto
.PHONY: all run clean
