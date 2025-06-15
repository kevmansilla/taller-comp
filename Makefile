# Nombre del archivo de salida
EXEC = test_lab
LAB2_EXEC = Lab2

# Archivos fuente
SRC = main.hs
LAB2_SRC = Lab2.hs

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

# Regla para compilar Lab2.hs
io: $(LAB2_SRC)
	@echo "Compilando Lab2.hs..."
	$(GHC) -o $(LAB2_EXEC) $(LAB2_SRC)
	@if [ $$? -ne 0 ]; then \
		echo "❌ Error de compilación en Lab2.hs"; \
		exit 1; \
	fi
	@echo "Compilación de Lab2.hs exitosa"
	./$(LAB2_EXEC)

# Regla para ejecutar los tests
test: $(EXEC)
	@echo "Ejecutando tests..."
	./$(EXEC)

# Limpiar archivos generados
clean:
	@echo "Limpiando archivos generados..."
	rm -f $(EXEC) $(LAB2_EXEC) *.hi *.o

# Regla por defecto
.PHONY: all run clean
