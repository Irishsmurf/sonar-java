/*
 * SonarQube Java
 * Copyright (C) 2012-2016 SonarSource SA
 * mailto:contact AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.java.resolve;

import javax.annotation.CheckForNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class TypeInferenceSolver {

  private final ParametrizedTypeCache parametrizedTypeCache;
  private final Symbols symbols;

  public TypeInferenceSolver(ParametrizedTypeCache parametrizedTypeCache, Symbols symbols) {
    this.parametrizedTypeCache = parametrizedTypeCache;
    this.symbols = symbols;
  }

  @CheckForNull
  TypeSubstitution inferTypes(JavaSymbol.MethodJavaSymbol method, JavaType site, List<JavaType> typeParams, List<JavaType> argTypes) {
    List<JavaType> formals = ((JavaType.MethodJavaType) method.type).argTypes;
    TypeSubstitution substitution = new TypeSubstitution();
    if (method.isParametrized()) {
      if (!typeParams.isEmpty()) {
        substitution = getSubstitutionFromTypeParams(method.typeVariableTypes, typeParams);
      } else {
        formals = applySubstitution(formals, site);
        substitution = getSubstitutionFromArguments(method, formals, argTypes);
      }
      if (substitution.size() == 0 || !isValidSubtitution(substitution, site)) {
        // substitution discarded
        return null;
      }
    }
    return substitution;
  }

  JavaType inferReturnType(JavaSymbol.MethodJavaSymbol method, JavaType site, List<JavaType> typeParams, List<JavaType> argTypes) {
    JavaType resultType = applySubstitution(((JavaType.MethodJavaType) method.type).resultType, site);
    TypeSubstitution substitution = inferTypes(method, site, typeParams, argTypes);
    if (substitution != null) {
      resultType = applySubstitution(resultType, substitution);
    }
    return resultType;
  }

  List<JavaType> applySubstitution(List<JavaType> types, JavaType site) {
    if (isParametrizedType(site)) {
      return applySubstitution(types, ((JavaType.ParametrizedTypeJavaType) site).typeSubstitution);
    }
    return types;
  }

  JavaType applySubstitution(JavaType type, JavaType site) {
    if (isParametrizedType(site)) {
      return applySubstitution(type, ((JavaType.ParametrizedTypeJavaType) site).typeSubstitution);
    }
    return type;
  }

  List<JavaType> applySubstitution(List<JavaType> types, TypeSubstitution substitution) {
    if (substitution.size() == 0) {
      return types;
    }
    List<JavaType> results = new ArrayList<>(types.size());
    for (JavaType type : types) {
      results.add(applySubstitution(type, substitution));
    }
    return results;
  }

  private JavaType applySubstitution(JavaType type, TypeSubstitution substitution) {
    JavaType substitutedType = substitution.substitutedType(type);
    if (substitutedType != null) {
      return substitutedType;
    }
    if (isParametrizedType(type)) {
      return applySubstitution((JavaType.ParametrizedTypeJavaType) type, substitution);
    }
    if (type.isTagged(JavaType.WILDCARD)) {
      return applySubstitution((JavaType.WildCardType) type, substitution);
    }
    if (type.isArray()) {
      return applySubstitution((JavaType.ArrayJavaType) type, substitution);
    }
    return type;
  }

  private static boolean isParametrizedType(JavaType type) {
    return type instanceof JavaType.ParametrizedTypeJavaType;
  }

  private JavaType applySubstitution(JavaType.ParametrizedTypeJavaType type, TypeSubstitution substitution) {
    TypeSubstitution newSubstitution = new TypeSubstitution();
    for (Map.Entry<JavaType.TypeVariableJavaType, JavaType> entry : type.typeSubstitution.substitutionEntries()) {
      newSubstitution.add(entry.getKey(), applySubstitution(entry.getValue(), substitution));
    }
    return parametrizedTypeCache.getParametrizedTypeType(type.rawType.getSymbol(), newSubstitution);
  }

  private JavaType applySubstitution(JavaType.WildCardType wildcard, TypeSubstitution substitution) {
    JavaType substitutedType = substitution.substitutedType(wildcard.bound);
    if (substitutedType != null) {
      return parametrizedTypeCache.getWildcardType(substitutedType, wildcard.boundType);
    }
    return wildcard;
  }

  private JavaType applySubstitution(JavaType.ArrayJavaType arrayType, TypeSubstitution substitution) {
    JavaType rootElementType = arrayType.elementType;
    int nbDimensions = 1;
    while (rootElementType.isArray()) {
      rootElementType = ((JavaType.ArrayJavaType) rootElementType).elementType;
      nbDimensions++;
    }
    JavaType substitutedType = substitution.substitutedType(rootElementType);
    if (substitutedType != null) {
      // FIXME SONARJAVA-1574 a new array type should not be created but reused if already existing for the current element type
      for (int i = 0; i < nbDimensions; i++) {
        substitutedType = new JavaType.ArrayJavaType(substitutedType, symbols.arrayClass);
      }
      return substitutedType;
    }
    return arrayType;
  }

  TypeSubstitution getSubstitutionFromTypeParams(List<JavaType.TypeVariableJavaType> typeVariableTypes, List<JavaType> typeParams) {
    TypeSubstitution substitution = new TypeSubstitution();
    if (typeVariableTypes.size() == typeParams.size()) {
      // create naive substitution
      for (int i = 0; i < typeVariableTypes.size(); i++) {
        JavaType.TypeVariableJavaType typeVariableType = typeVariableTypes.get(i);
        JavaType typeParam = typeParams.get(i);
        substitution.add(typeVariableType, typeParam);
      }
    }
    return substitution;
  }

  private TypeSubstitution getSubstitutionFromArguments(JavaSymbol.MethodJavaSymbol method, List<JavaType> formals, List<JavaType> argTypes) {
    TypeSubstitution substitution = new TypeSubstitution();

    if (formals.size() > argTypes.size() || formals.isEmpty() || argTypes.isEmpty()) {
      // no need to try to infer types
      return substitution;
    }

    List<JavaType.TypeVariableJavaType> typeVariablesToInfer = new ArrayList<>(method.typeVariableTypes);
    // compete with type variable from the scope
    for (JavaType argType : argTypes) {
      JavaSymbol owner = argType.symbol.owner;
      if (owner.isMethodSymbol() && ((JavaSymbol.MethodJavaSymbol) owner).typeVariableTypes.contains(argType)) {
        typeVariablesToInfer.add((JavaType.TypeVariableJavaType) argType);
      }
    }

    if (!typeVariablesToInfer.isEmpty()) {
      for (int i = 0; i < formals.size(); i++) {
        JavaType formalType = formals.get(i);
        JavaType argType = argTypes.get(i);

        if (formalType.isTagged(JavaType.TYPEVAR)) {
          completeSubstitution(substitution, formalType, argType);
        } else if (formalType.isArray() && argType.isArray()) {
          completeSubstitution(substitution, ((JavaType.ArrayJavaType) formalType).elementType, ((JavaType.ArrayJavaType) argType).elementType);
        } else if (formalType.isArray() && method.isVarArgs() && i == formals.size() - 1) {
          completeSubstitution(substitution, ((JavaType.ArrayJavaType) formalType).elementType, argType);
        } else if (isParametrizedType(formalType) && isParametrizedType(argType)) {
          List<JavaType> formalTypeSubstitutedTypes = ((JavaType.ParametrizedTypeJavaType) formalType).typeSubstitution.substitutedTypes();
          List<JavaType> argTypeSubstitutedTypes = ((JavaType.ParametrizedTypeJavaType) argType).typeSubstitution.substitutedTypes();
          TypeSubstitution newSubstitution = getSubstitutionFromArguments(method, formalTypeSubstitutedTypes, argTypeSubstitutedTypes);
          substitution = mergeTypeSubstitutions(substitution, newSubstitution);
        } else if (isParametrizedType(formalType) && !isParametrizedType(argType) && formalType.erasure() == argType) {
          List<JavaType> formalTypeSubstitutedTypes = ((JavaType.ParametrizedTypeJavaType) formalType).typeSubstitution.substitutedTypes();
          List<JavaType> fakeTypes = new ArrayList<>(formalTypeSubstitutedTypes.size());
          for (int j = 0; j < formalTypeSubstitutedTypes.size(); j++) {
            fakeTypes.add(symbols.objectType);
          }
          TypeSubstitution newSubstitution = getSubstitutionFromArguments(method, formalTypeSubstitutedTypes, fakeTypes);
          substitution = mergeTypeSubstitutions(substitution, newSubstitution);
        } else if (formalType.isTagged(JavaType.WILDCARD)) {
          completeSubstitution(substitution, ((JavaType.WildCardType) formalType).bound, argType);
        }

        if (substitution.typeVariables().containsAll(method.typeVariableTypes)) {
          // we found all the substitution
          break;
        }
      }
    }
    return substitution;
  }

  private static TypeSubstitution mergeTypeSubstitutions(TypeSubstitution currentSubstitution, TypeSubstitution newSubstitution) {
    TypeSubstitution result = new TypeSubstitution();
    for (Map.Entry<JavaType.TypeVariableJavaType, JavaType> substitution : currentSubstitution.substitutionEntries()) {
      result.add(substitution.getKey(), substitution.getValue());
    }
    for (Map.Entry<JavaType.TypeVariableJavaType, JavaType> substitution : newSubstitution.substitutionEntries()) {
      if (!result.typeVariables().contains(substitution.getKey())) {
        result.add(substitution.getKey(), substitution.getValue());
      }
    }
    return result;
  }

  private static void completeSubstitution(TypeSubstitution currentSubstitution, JavaType formalType, JavaType argType) {
    if (formalType.isTagged(JavaType.TYPEVAR) && currentSubstitution.substitutedType(formalType) == null) {
      JavaType expectedType = argType;
      if (expectedType.isPrimitive()) {
        expectedType = expectedType.primitiveWrapperType;
      }
      JavaType.TypeVariableJavaType typeVar = (JavaType.TypeVariableJavaType) formalType;
      currentSubstitution.add(typeVar, expectedType);
    }
  }

  private static boolean isValidSubtitution(TypeSubstitution substitutions, JavaType site) {
    for (Map.Entry<JavaType.TypeVariableJavaType, JavaType> substitution : substitutions.substitutionEntries()) {
      if (!isValidSubstitution(substitutions, substitution.getKey(), substitution.getValue(), site)) {
        return false;
      }
    }
    return true;
  }

  private static boolean isValidSubstitution(TypeSubstitution candidate, JavaType.TypeVariableJavaType typeVar, JavaType typeParam, JavaType site) {
    for (JavaType bound : typeVar.bounds) {
      while (bound.isTagged(JavaType.TYPEVAR)) {
        JavaType newBound = candidate.substitutedType(bound);
        if (newBound == null && isParametrizedType(site)) {
          newBound = ((JavaType.ParametrizedTypeJavaType) site).typeSubstitution.substitutedType(bound);
        }
        if (newBound == null) {
          return false;
        }
        bound = newBound;
      }
      if (!typeParam.isSubtypeOf(bound)) {
        return false;
      }
    }
    return true;
  }

}
